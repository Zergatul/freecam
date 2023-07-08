package com.zergatul.freecam;

import com.zergatul.freecam.math.Quaternion;
import com.zergatul.freecam.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.AbstractClientPlayer;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.MovementInput;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import org.lwjgl.opengl.GL11;

import java.util.List;

public class FreeCam {

    public static final FreeCam instance = new FreeCam();

    private final Minecraft mc = Minecraft.getMinecraft();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamPath path = new FreeCamPath(this);
    private final FreeCamConfig config = ConfigRepository.instance.load();
    private boolean active;
    private int oldCameraType;
    private MovementInput playerInput;
    private MovementInput freecamInput;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private boolean cameraLock;
    private boolean moveAlongPath;
    private long pathStartTime;

    private FreeCam() {

    }

    public boolean isActive() {
        return active;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public double getZ() {
        return z;
    }

    public float getXRot() {
        return xRot;
    }

    public float getYRot() {
        return yRot;
    }

    public FreeCamConfig getConfig() {
        return config;
    }

    public FreeCamPath getPath() {
        return path;
    }

    public void toggle() {
        if (active) {
            disable();
        } else {
            enable();
        }
    }

    public void toggleCameraLock() {
        if (active) {
            cameraLock = !cameraLock;
            if (cameraLock) {
                mc.player.movementInput = playerInput;
            } else {
                mc.player.movementInput = freecamInput;
            }
        }
    }

    public void onKeyInput() {
        if (mc.player == null) {
            return;
        }
        if (mc.currentScreen != null) {
            return;
        }
        while (KeyBindings.toggleFreeCam.isPressed()) {
            toggle();
        }
        while (KeyBindings.toggleCameraLock.isPressed()) {
            toggleCameraLock();
        }
        while (KeyBindings.startPath.isPressed()) {
            startPath();
        }
    }

    public void onMouseTurn(double yRot, double xRot, Runnable callSuper) {
        if (active) {
            if (!moveAlongPath) {
                if (cameraLock) {
                    if (override != null) {
                        restoreCameraEntityPosition();
                    }
                    callSuper.run();
                    if (override != null) {
                        saveCameraEntityPosition();
                        moveCameraEntityToFreeCamPosition();
                    }
                } else {
                    this.xRot += (float) xRot * 0.15F;
                    this.yRot += (float) yRot * 0.15F;
                    this.xRot = MathHelper.clamp(this.xRot, -90, 90);
                    calculateVectors();
                }
            }
        } else {
            callSuper.run();
        }
    }

    public void onClientTickStart() {
        if (active) {
            while (mc.gameSettings.keyBindTogglePerspective.isPressed()) {
                // consume clicks
            }
            playerInput.updatePlayerMoveState();
        }
    }

    public void onRenderTickStart() {
        if (active) {
            long currTime = System.nanoTime();
            float frameTime = (currTime - lastTime) / 1e9f;
            lastTime = currTime;

            if (moveAlongPath) {
                FreeCamPath.Entry entry = path.interpolate((currTime - pathStartTime) / 1e6);
                if (entry == null) {
                    moveAlongPath = false;
                } else {
                    x = entry.position.x;
                    y = entry.position.y;
                    z = entry.position.z;
                    xRot = (float) entry.xRot;
                    yRot = (float) entry.yRot;
                }
            } else {
                MovementInput input = playerInput;
                float forwardImpulse = cameraLock ? 0 : input.moveForward;
                float leftImpulse = cameraLock ? 0 : input.moveStrafe;
                float upImpulse = cameraLock ? 0 : (input.jump ? 1 : 0) + (input.sneak ? -1 : 0);
                double slowdown = Math.pow(config.slowdownFactor, frameTime);
                forwardVelocity = combineMovement(forwardVelocity, forwardImpulse, frameTime, config.acceleration, slowdown);
                leftVelocity = combineMovement(leftVelocity, leftImpulse, frameTime, config.acceleration, slowdown);
                upVelocity = combineMovement(upVelocity, upImpulse, frameTime, config.acceleration, slowdown);

                double dx = (double) this.forwards.x * forwardVelocity + (double) this.left.x * leftVelocity;
                double dy = (double) this.forwards.y * forwardVelocity + upVelocity + (double) this.left.y * leftVelocity;
                double dz = (double) this.forwards.z * forwardVelocity + (double) this.left.z * leftVelocity;
                dx *= frameTime;
                dy *= frameTime;
                dz *= frameTime;
                double speed = Math.sqrt(dx * dx + dy * dy + dz * dz) / frameTime;
                if (speed > config.maxSpeed) {
                    double factor = config.maxSpeed / speed;
                    forwardVelocity *= factor;
                    leftVelocity *= factor;
                    upVelocity *= factor;
                    dx *= factor;
                    dy *= factor;
                    dz *= factor;
                }
                x += dx;
                y += dy;
                z += dz;
            }
        }
    }

    public void onWorldUnload() {
        disable();
    }

    public void onGetDebugInfoLeft(List<String> list) {
        if (active) {
            list.add("");
            list.add(String.format("FreeCam XYZ: %.3f / %.5f / %.3f", x, y, z));
        }
    }

    private double px, py, pz, lastX, lastY, lastZ, llX, llY, llZ;
    private float eXRot, eYRot, lastXRot, lastYRot;
    private AxisAlignedBB lastBB;
    private boolean pNoClip;
    private Entity override;
    private boolean entitiesRendering;

    public void onBeforeRenderWorld() {
        override = null;

        if (!active) {
            return;
        }

        Entity cameraEntity = mc.getRenderViewEntity();
        if (cameraEntity == null) {
            return;
        }

        override = cameraEntity;
        saveCameraEntityPosition();
        moveCameraEntityToFreeCamPosition();
        pNoClip = override.noClip;
        override.noClip = true;
    }

    public void onAfterRenderWorld() {
        if (override == null) {
            return;
        }

        restoreCameraEntityPosition();
        override.noClip = pNoClip;
        override = null;
    }

    public void onBeforeRenderEntity(Entity entity) {
        if (override == entity) {
            restoreCameraEntityPosition();
        }
    }

    public void onAfterRenderEntity(Entity entity) {
        if (override == entity) {
            moveCameraEntityToFreeCamPosition();
        }
    }

    public void onBeforeRenderEntities() {
        entitiesRendering = true;
        if (override != null) {
            mc.gameSettings.thirdPersonView = 1;
        }
    }

    public void onAfterRenderEntities() {
        entitiesRendering = false;
        if (override != null) {
            mc.gameSettings.thirdPersonView = 0;
        }
    }

    public double getViewFrustumEntityPosX(double viewEntityX) {
        return override != null ? px : viewEntityX;
    }

    public double getViewFrustumEntityPosZ(double viewEntityZ) {
        return override != null ? pz : viewEntityZ;
    }

    public boolean shouldOverrideSpectator(AbstractClientPlayer player) {
        if (override == player && !entitiesRendering) {
            return true;
        } else {
            return false;
        }
    }

    public boolean shouldRenderHands() {
        if (!active) {
            return true;
        } else {
            return !cameraLock && config.renderHands;
        }
    }

    public void onRunTickBeforeCalcHitResult() {
        if (active && !cameraLock && config.target) {
            Entity cameraEntity = mc.getRenderViewEntity();
            if (cameraEntity == null) {
                return;
            }

            override = cameraEntity;
            saveCameraEntityPosition();
            moveCameraEntityToFreeCamPosition();
        }
    }

    public void onRunTickAfterCalcHitResult() {
        if (active && !cameraLock && config.target) {
            if (override != null) {
                restoreCameraEntityPosition();
                override = null;
            }
        }
    }

    public void onRenderWorldBeforeCalcHitResult() {
        if (active && (!config.target || cameraLock)) {
            if (override != null) {
                restoreCameraEntityPosition();
            }
        }
    }

    public void onRenderWorldAfterCalcHitResult() {
        if (active && (!config.target || cameraLock)) {
            if (override != null) {
                moveCameraEntityToFreeCamPosition();
            }
        }
    }

    public boolean shouldRenderCrosshair() {
        if (active) {
            if (cameraLock) {
                return false;
            }
            return config.target;
        } else {
            return true;
        }
    }

    public boolean shouldDisableBobbing() {
        return active && cameraLock;
    }

    public void onRenderWorldLast() {
        if (!active || moveAlongPath) {
            return;
        }

        List<FreeCamPath.Entry> path = getPath().get();
        if (path.size() < 2) {
            return;
        }

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder bufferBuilder = tessellator.getBuffer();

        {
            GlStateManager.pushMatrix();

            GL11.glPushAttrib(GL11.GL_ENABLE_BIT);
            GL11.glDisable(GL11.GL_CULL_FACE);
            GL11.glDisable(GL11.GL_LIGHTING);
            GL11.glDisable(GL11.GL_TEXTURE_2D);
            GL11.glDisable(GL11.GL_DEPTH_TEST);
            GL11.glEnable(GL11.GL_BLEND);
            GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);

            GL11.glLineWidth(1);
            GL11.glDepthMask(false);
        }

        float eyeHeight = mc.getRenderViewEntity() != null ? mc.getRenderViewEntity().getEyeHeight() : 0;

        bufferBuilder.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION);
        GlStateManager.color(1f, 1f, 1f, 1f);

        for (int i = 1; i < path.size(); i++) {
            FreeCamPath.Entry e1 = path.get(i - 1);
            FreeCamPath.Entry e2 = path.get(i);

            bufferBuilder.pos(e1.position.x - x, e1.position.y - y + eyeHeight, e1.position.z - z).endVertex();
            bufferBuilder.pos(e2.position.x - x, e2.position.y - y + eyeHeight, e2.position.z - z).endVertex();
        }

        tessellator.draw();

        {
            GL11.glDepthMask(true);
            GL11.glPopAttrib();
            GlStateManager.popMatrix();
        }
    }

    private void enable() {
        if (active) {
            return;
        }

        Entity entity = mc.getRenderViewEntity();
        if (entity == null) {
            return;
        }

        active = true;
        cameraLock = false;
        moveAlongPath = false;
        oldCameraType = mc.gameSettings.thirdPersonView;
        playerInput = mc.player.movementInput;
        mc.player.movementInput = freecamInput = new MovementInput();
        mc.gameSettings.thirdPersonView = 0;

        Vec3d pos = entity.getPositionEyes(1);
        x = pos.x;
        y = pos.y;
        z = pos.z;
        yRot = entity.rotationYaw;
        xRot = entity.rotationPitch;

        calculateVectors();

        double distance = -2;
        x += (double)this.forwards.x * distance;
        y += (double)this.forwards.y * distance;
        z += (double)this.forwards.z * distance;

        forwardVelocity = 0;
        leftVelocity = 0;
        upVelocity = 0;

        lastTime = System.nanoTime();
    }

    private void disable() {
        if (!active) {
            return;
        }

        active = false;
        mc.gameSettings.thirdPersonView = oldCameraType;
        mc.player.movementInput = playerInput;
    }

    private void startPath() {
        if (!active) {
            return;
        }

        moveAlongPath = true;
        pathStartTime = System.nanoTime();
    }

    private void calculateVectors() {
        rotation.set(0.0F, 0.0F, 0.0F, 1.0F);
        rotation.mul(Vector3f.YP.rotationDegrees(-yRot));
        rotation.mul(Vector3f.XP.rotationDegrees(xRot));
        forwards.set(0.0F, 0.0F, 1.0F);
        forwards.transform(rotation);
        up.set(0.0F, 1.0F, 0.0F);
        up.transform(rotation);
        left.set(1.0F, 0.0F, 0.0F);
        left.transform(rotation);
    }

    private double combineMovement(double velocity, double impulse, double frameTime, double acceleration, double slowdown) {
        if (impulse != 0) {
            if (impulse > 0 && velocity < 0) {
                velocity = 0;
            }
            if (impulse < 0 && velocity > 0) {
                velocity = 0;
            }
            velocity += acceleration * impulse * frameTime;
        } else {
            velocity *= slowdown;
        }
        return velocity;
    }

    private void saveCameraEntityPosition() {
        px = override.posX;
        py = override.posY;
        pz = override.posZ;
        lastX = override.lastTickPosX;
        lastY = override.lastTickPosY;
        lastZ = override.lastTickPosZ;
        llX = override.prevPosX;
        llY = override.prevPosY;
        llZ = override.prevPosZ;
        eXRot = override.rotationPitch;
        eYRot = override.rotationYaw;
        lastXRot = override.prevRotationPitch;
        lastYRot = override.prevRotationYaw;
        lastBB = override.getEntityBoundingBox();
    }

    private void restoreCameraEntityPosition() {
        override.posX = px;
        override.posY = py;
        override.posZ = pz;
        override.lastTickPosX = lastX;
        override.lastTickPosY = lastY;
        override.lastTickPosZ = lastZ;
        override.prevPosX = llX;
        override.prevPosY = llY;
        override.prevPosZ = llZ;
        override.rotationPitch = eXRot;
        override.rotationYaw = eYRot;
        override.prevRotationPitch = lastXRot;
        override.prevRotationYaw = lastYRot;
        override.setEntityBoundingBox(lastBB);
    }

    private void moveCameraEntityToFreeCamPosition() {
        override.posX = override.lastTickPosX = override.prevPosX = x;
        override.posY = override.lastTickPosY = override.prevPosY = y;
        override.posZ = override.lastTickPosZ = override.prevPosZ = z;
        override.rotationPitch = override.prevRotationPitch = xRot;
        override.rotationYaw = override.prevRotationYaw = yRot;

        double hw = override.width / 2;
        override.setEntityBoundingBox(new AxisAlignedBB(
                override.posX - hw, override.posY, override.posZ - hw,
                override.posX + hw, override.posY + override.height, override.posZ + hw));
    }
}