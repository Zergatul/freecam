package com.zergatul.freecam;

import com.zergatul.freecam.math.Quaternion;
import com.zergatul.freecam.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.AbstractClientPlayer;
import net.minecraft.entity.Entity;
import net.minecraft.util.MovementInput;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;

import java.util.List;
import java.util.concurrent.TimeUnit;

public class FreeCam {

    public static final FreeCam instance = new FreeCam();

    private static final int REMEMBER_STATE_DELAY_MS = 400;

    private final Minecraft mc = Minecraft.getMinecraft();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamConfig config = ConfigRepository.instance.load();
    private boolean active;
    private int oldCameraType;
    private MovementInput playerInput;
    private MovementInput freecamInput;
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

    private FreeCam() {

    }

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
        if (mc.player == null || mc.currentScreen != null) {
            return;
        }

        while (KeyBindings.toggleFreeCam.isPressed()) {
            toggle();
        }
        while (KeyBindings.toggleCameraLock.isPressed()) {
            toggleCameraLock();
        }
        while (KeyBindings.toggleEyeLock.isPressed()) {
            toggleEyeLock();
        }
        while (KeyBindings.toggleFollowCam.isPressed()) {
            toggleFollowCamera();
        }
    }

    public boolean onMouseTurn(double yRot, double xRot) {
        if (active && !cameraLock && !followCamera) {
            if (!eyeLock) {
                this.xRot += (float) xRot * 0.15F;
                this.yRot += (float) yRot * 0.15F;
                this.xRot = MathHelper.clamp(this.xRot, -90, 90);
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
            if (mc.player != null && mc.player.movementInput != playerInput) {
                playerInput.updatePlayerMoveState();
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
        return !active || !cameraLock && !eyeLock && !followCamera && config.renderHands;
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
        return active && !cameraLock && !eyeLock && !followCamera && config.target &&
                picking && entity == mc.getRenderViewEntity();
    }

    public Vec3d getTargetLookVector() {
        float yawCos = MathHelper.cos(-yRot * 0.017453292F - (float) Math.PI);
        float yawSin = MathHelper.sin(-yRot * 0.017453292F - (float) Math.PI);
        float pitchCos = -MathHelper.cos(-xRot * 0.017453292F);
        float pitchSin = MathHelper.sin(-xRot * 0.017453292F);
        return new Vec3d(yawSin * pitchCos, pitchSin, yawCos * pitchCos);
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
        if (!active) {
            return;
        }

        long currTime = System.nanoTime();
        float frameTime = (currTime - lastTime) / 1e9f;
        lastTime = currTime;

        if (followCamera) {
            Entity entity = mc.getRenderViewEntity();
            if (entity != null) {
                Vec3d pos = entity.getPositionEyes(partialTicks);
                x = pos.x + followDeltaX;
                y = pos.y + followDeltaY;
                z = pos.z + followDeltaZ;
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

            double dx = (double) forwards.x * forwardVelocity + (double) left.x * leftVelocity;
            double dy = (double) forwards.y * forwardVelocity + upVelocity + (double) left.y * leftVelocity;
            double dz = (double) forwards.z * forwardVelocity + (double) left.z * leftVelocity;
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

    public void onWorldUnload() {
        disable();
    }

    public void onGetDebugInfoLeft(List<String> list) {
        if (active) {
            list.add("");
            list.add("FreeCam");
            list.add(String.format("XYZ: %.3f / %.5f / %.3f", x, y, z));
            list.add(String.format("Facing: (%.1f / %.1f)",
                    MathHelper.wrapDegrees(yRot),
                    MathHelper.wrapDegrees(xRot)));
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
        return override == player && !entitiesRendering;
    }

    public double getViewFrustumEntityPosX(double viewEntityX) {
        return override != null ? px : viewEntityX;
    }

    public double getViewFrustumEntityPosZ(double viewEntityZ) {
        return override != null ? pz : viewEntityZ;
    }

    public boolean shouldDisableBobbing() {
        return active;
    }

    private void enable() {
        if (active) {
            return;
        }

        Entity entity = mc.getRenderViewEntity();
        if (entity == null || mc.player == null) {
            return;
        }

        active = true;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        oldCameraType = mc.gameSettings.thirdPersonView;
        playerInput = mc.player.movementInput;
        playerInput.updatePlayerMoveState();
        mc.player.movementInput = freecamInput = createFreeCamInput(playerInput);
        mc.gameSettings.thirdPersonView = 0;

        if (config.rememberInputState) {
            dontMoveFreeCamBefore = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(REMEMBER_STATE_DELAY_MS);
        }

        oldEntity = entity;
        Vec3d pos = entity.getPositionEyes(1);
        x = pos.x;
        y = pos.y;
        z = pos.z;
        yRot = entity.rotationYaw;
        xRot = entity.rotationPitch;

        calculateVectors();

        double distance = -2;
        x += (double) forwards.x * distance;
        y += (double) forwards.y * distance;
        z += (double) forwards.z * distance;

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
        if (mc.player != null) {
            mc.player.movementInput = playerInput;
        }
        if (oldEntity != null) {
            mc.setRenderViewEntity(oldEntity);
        }
    }

    private MovementInput createFreeCamInput(MovementInput input) {
        MovementInput result = new MovementInput();
        if (config.rememberInputState) {
            result.moveForward = input.moveForward;
            result.moveStrafe = input.moveStrafe;
            result.jump = input.jump;
            result.sneak = input.sneak;
        }
        return result;
    }

    private void toggleCameraLock() {
        if (active && !followCamera) {
            cameraLock = !cameraLock;
            mc.player.movementInput = cameraLock ? playerInput : freecamInput;
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

            mc.player.movementInput = playerInput;
            cameraLock = false;
            eyeLock = false;

            Vec3d pos = entity.getPositionEyes(1);
            followDeltaX = x - pos.x;
            followDeltaY = y - pos.y;
            followDeltaZ = z - pos.z;
        } else {
            mc.player.movementInput = freecamInput;
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

        Vec3d pos = entity.getPositionEyes(partialTicks);
        double dx = x - pos.x;
        double dy = y - pos.y;
        double dz = z - pos.z;
        xRot = (float) (Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180);
        yRot = (float) (Math.atan2(dz, dx) / Math.PI * 180 + 90);
        xRot = MathHelper.clamp(xRot, -90, 90);
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