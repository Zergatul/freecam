package com.zergatul.freecam;

import com.zergatul.freecam.math.Quaternion;
import com.zergatul.freecam.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.AbstractClientPlayer;
import net.minecraft.entity.Entity;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MathHelper;
import net.minecraft.util.MovementInput;
import net.minecraft.util.Vec3;

import java.util.List;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private final Minecraft mc = Minecraft.getMinecraft();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private FreeCamConfig config = new FreeCamConfig(); //ConfigStore.instance.load();
    private boolean active;
    private int oldCameraType;
    private MovementInput oldInput;
    private Entity oldEntity;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;

    private FreeCamController() {

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
        if (KeyBindingsController.toggleFreeCam.isKeyDown()) {
            toggle();
        }
    }

    public void onMouseTurn(double yRot, double xRot) {
        this.xRot += (float) xRot * 0.15F;
        this.yRot += (float) yRot * 0.15F;
        this.xRot = MathHelper.clamp_float(this.xRot, -90, 90);
        calculateVectors();
    }

    public void onClientTickStart() {
        if (active) {
            while (mc.gameSettings.keyBindTogglePerspective.isPressed()) {
                // consume clicks
            }
            oldInput.updatePlayerMoveState();
        }
    }

    public void onRenderTickStart() {
        if (active) {
            long currTime = System.nanoTime();
            float frameTime = (currTime - lastTime) / 1e9f;
            lastTime = currTime;

            MovementInput input = oldInput;
            float forwardImpulse = input.moveForward;
            float leftImpulse = input.moveStrafe;
            float upImpulse = (input.jump ? 1 : 0) + (input.sneak ? -1 : 0);
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

    private void enable() {
        if (active) {
            return;
        }

        active = true;
        oldCameraType = mc.gameSettings.thirdPersonView;
        oldInput = mc.thePlayer.movementInput;
        mc.thePlayer.movementInput = new MovementInput();
        mc.gameSettings.thirdPersonView = 0;

        oldEntity = mc.getRenderViewEntity();
        //float partialTicks = mc.timer.partialTicks;
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
        //fakeEntity = null;
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
        override.posY = override.lastTickPosY = override.prevPosY = y;
        override.posZ = override.lastTickPosZ = override.prevPosZ = z;
        override.rotationPitch = override.prevRotationPitch = xRot;
        override.rotationYaw = override.prevRotationYaw = yRot;
    }
}