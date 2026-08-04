package com.zergatul.freecam;

import com.zergatul.freecam.math.Quaternion;
import com.zergatul.freecam.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.AbstractClientPlayer;
import net.minecraft.entity.Entity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.MathHelper;
import net.minecraft.util.MovementInput;
import net.minecraft.util.Vec3;

import java.util.List;
import java.util.concurrent.TimeUnit;

public class FreeCam {

    public static final FreeCam INSTANCE = new FreeCam();

    private static final int REMEMBER_STATE_DELAY_MS = 400;

    private final Minecraft mc = Minecraft.getMinecraft();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamConfig config = ConfigRepository.INSTANCE.getConfig();
    private boolean active;
    private int oldCameraType;
    private MovementInput oldInput;
    private MovementInput freeCamInput;
    private Entity oldEntity;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private long dontMoveFreeCamBefore;
    private boolean picking;
    private boolean cameraRestoredForPicking;
    private boolean cameraLock;
    private boolean eyeLock;
    private boolean followCamera;
    private double followDeltaX, followDeltaY, followDeltaZ;

    private FreeCam() {}

    public boolean isActive() {
        return active;
    }

    public FreeCamConfig getConfig() {
        return config;
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

    public void toggle() {
        if (active) {
            disable();
        } else {
            enable();
        }
    }

    public void onKeyInput() {
        if (mc.thePlayer == null) {
            return;
        }
        if (mc.currentScreen != null) {
            return;
        }
        while (KeyBindingsController.toggleFreeCam.isPressed()) {
            toggle();
        }
        while (KeyBindingsController.toggleCameraLock.isPressed()) {
            toggleCameraLock();
        }
        while (KeyBindingsController.toggleEyeLock.isPressed()) {
            toggleEyeLock();
        }
        while (KeyBindingsController.toggleFollowCam.isPressed()) {
            toggleFollowCamera();
        }
    }

    public boolean onMouseTurn(double yRot, double xRot) {
        if (active && !cameraLock && !followCamera) {
            if (!eyeLock) {
                this.xRot += (float) xRot * 0.15F;
                this.yRot += (float) yRot * 0.15F;
                this.xRot = MathHelper.clamp_float(this.xRot, -90, 90);
                calculateVectors();
            }
            return false;
        }

        return true;
    }

    public void onClientTickStart() {
        if (active) {
            while (mc.gameSettings.keyBindTogglePerspective.isPressed()) {
                // consume clicks
            }
            if (mc.thePlayer != null && mc.thePlayer.movementInput != oldInput) {
                oldInput.updatePlayerMoveState();
            }
        }
    }

    public boolean shouldShowMyName() {
        return active && config.showMyName;
    }

    public boolean shouldRenderTarget() {
        return !active || !cameraLock && !eyeLock && !followCamera && config.target;
    }

    public boolean shouldRenderHands() {
        return active && !cameraLock && !eyeLock && !followCamera && config.renderHands;
    }

    public void onBeforePick() {
        picking = true;
        cameraRestoredForPicking = false;

        if (override != null) {
            restoreCameraEntityPosition();
            cameraRestoredForPicking = true;
        }
    }

    public void onAfterPick() {
        if (cameraRestoredForPicking && override != null) {
            moveCameraEntityToFreeCamPosition();
        }

        cameraRestoredForPicking = false;
        picking = false;
    }

    public boolean shouldOverrideCameraEntityForPicking(Entity entity) {
        return active && !cameraLock && !eyeLock && !followCamera && config.target && picking && entity == mc.getRenderViewEntity();
    }

    public Vec3 getTargetLookVector() {
        float yawCos = MathHelper.cos(-yRot * 0.017453292F - (float)Math.PI);
        float yawSin = MathHelper.sin(-yRot * 0.017453292F - (float)Math.PI);
        float pitchCos = -MathHelper.cos(-xRot * 0.017453292F);
        float pitchSin = MathHelper.sin(-xRot * 0.017453292F);
        return new Vec3((double)(yawSin * pitchCos), (double)pitchSin, (double)(yawCos * pitchCos));
    }

    public AxisAlignedBB getTargetSearchBox(Entity entity, AxisAlignedBB box) {
        if (!shouldOverrideCameraEntityForPicking(entity)) {
            return box;
        }

        double dx = x - entity.posX;
        double dy = y - (entity.posY + entity.getEyeHeight());
        double dz = z - entity.posZ;
        return box.offset(dx, dy, dz);
    }

    public void onRenderTickStart(float partialTicks) {
        if (active) {
            long currTime = System.nanoTime();
            float frameTime = (currTime - lastTime) / 1e9f;
            lastTime = currTime;

            if (followCamera) {
                Entity entity = mc.getRenderViewEntity();
                if (entity != null) {
                    Vec3 pos = entity.getPositionEyes(partialTicks);
                    x = pos.xCoord + followDeltaX;
                    y = pos.yCoord + followDeltaY;
                    z = pos.zCoord + followDeltaZ;
                }
            } else {
                MovementInput input = oldInput;
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
                if (!config.rememberInputState || currTime >= dontMoveFreeCamBefore) {
                    x += dx;
                    y += dy;
                    z += dz;
                }
            }

            applyEyeLock(partialTicks);
        }
    }

    public void onGetDebugInfoLeft(List<String> list) {
        if (active) {
            list.add("");
            list.add("FreeCam");
            list.add(String.format("XYZ: %.3f / %.5f / %.3f", x, y, z));
            list.add(String.format("Facing: (%.1f / %.1f)", MathHelper.wrapAngleTo180_float(yRot), MathHelper.wrapAngleTo180_float(xRot)));
        }
    }

    private double px, py, pz, lastX, lastY, lastZ, llX, llY, llZ;
    private float eXRot, eYRot, lastXRot, lastYRot;
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

    public boolean shouldOverrideSpectator(AbstractClientPlayer player) {
        if (override == player && !entitiesRendering) {
            return true;
        } else {
            return false;
        }
    }

    public double getViewFrustumEntityPosX(double viewEntityX) {
        return override != null ? px : viewEntityX;
    }

    public double getViewFrustumEntityPosZ(double viewEntityZ) {
        return override != null ? pz : viewEntityZ;
    }

    private void enable() {
        if (active) {
            return;
        }

        active = true;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        oldCameraType = mc.gameSettings.thirdPersonView;
        oldInput = mc.thePlayer.movementInput;
        oldInput.updatePlayerMoveState();
        mc.thePlayer.movementInput = freeCamInput = createFreeCamInput(oldInput);
        mc.gameSettings.thirdPersonView = 0;

        if (config.rememberInputState) {
            dontMoveFreeCamBefore = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(REMEMBER_STATE_DELAY_MS);
        }

        oldEntity = mc.getRenderViewEntity();
        Vec3 pos = oldEntity.getPositionEyes(1);
        x = pos.xCoord;
        y = pos.yCoord;
        z = pos.zCoord;
        yRot = oldEntity.rotationYaw;
        xRot = oldEntity.rotationPitch;

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
        mc.thePlayer.movementInput = oldInput;

        mc.setRenderViewEntity(oldEntity);
    }

    private MovementInput createFreeCamInput(MovementInput playerInput) {
        MovementInput freeCamInput = new MovementInput();
        if (config.rememberInputState) {
            freeCamInput.moveForward = playerInput.moveForward;
            freeCamInput.moveStrafe = playerInput.moveStrafe;
            freeCamInput.jump = playerInput.jump;
            freeCamInput.sneak = playerInput.sneak;
        }
        return freeCamInput;
    }

    private void toggleCameraLock() {
        if (active && !followCamera) {
            cameraLock = !cameraLock;
            mc.thePlayer.movementInput = cameraLock ? oldInput : freeCamInput;
        }
    }

    private void toggleEyeLock() {
        if (active && !followCamera) {
            eyeLock = !eyeLock;
        }
    }

    private void toggleFollowCamera() {
        if (!active) {
            return;
        }

        followCamera = !followCamera;
        if (followCamera) {
            Entity entity = mc.getRenderViewEntity();
            if (entity == null) {
                followCamera = false;
                return;
            }

            mc.thePlayer.movementInput = oldInput;
            cameraLock = false;
            eyeLock = false;

            Vec3 pos = entity.getPositionEyes(1);
            followDeltaX = x - pos.xCoord;
            followDeltaY = y - pos.yCoord;
            followDeltaZ = z - pos.zCoord;
        } else {
            mc.thePlayer.movementInput = freeCamInput;
        }
    }

    private void applyEyeLock(float partialTicks) {
        if (!eyeLock) {
            return;
        }

        Entity entity = mc.getRenderViewEntity();
        if (entity == null) {
            return;
        }

        Vec3 pos = entity.getPositionEyes(partialTicks);
        double dx = x - pos.xCoord;
        double dy = y - pos.yCoord;
        double dz = z - pos.zCoord;
        xRot = (float)(Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180);
        yRot = (float)(Math.atan2(dz, dx) / Math.PI * 180 + 90);
        xRot = MathHelper.clamp_float(xRot, -90, 90);
        calculateVectors();
    }

    private void calculateVectors() {
        rotation.set(0.0F, 0.0F, 0.0F, 1.0F);
        rotation.mul(Vector3f.YP.rotationDegrees(-yRot));
        if (!config.spectatorMovement) {
            rotation.mul(Vector3f.XP.rotationDegrees(xRot));
        }
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
    }

    private void moveCameraEntityToFreeCamPosition() {
        override.posX = override.lastTickPosX = override.prevPosX = x;
        override.posY = override.lastTickPosY = override.prevPosY = y - override.getEyeHeight();
        override.posZ = override.lastTickPosZ = override.prevPosZ = z;
        override.rotationPitch = override.prevRotationPitch = xRot;
        override.rotationYaw = override.prevRotationYaw = yRot;
    }
}