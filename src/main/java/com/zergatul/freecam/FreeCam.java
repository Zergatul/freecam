package com.zergatul.freecam;

import com.zergatul.freecam.math.Quaternion;
import com.zergatul.freecam.math.Vector3f;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.ChatComponentText;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.MathHelper;
import net.minecraft.util.MovementInput;
import net.minecraft.util.Vec3;

import java.util.concurrent.TimeUnit;

public class FreeCam {

    public static final FreeCam INSTANCE = new FreeCam();

    private static final int REMEMBER_STATE_DELAY_MS = 400;
    private static final double MAX_FRAME_TIME_SECONDS = 0.25D;

    private final Minecraft minecraft = Minecraft.getMinecraft();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);

    private boolean active;
    private EntityPlayerSP player;
    private EntityLivingBase cameraEntity;
    private MovementInput playerInput;
    private MovementInput freeCamPlayerInput;
    private int previousPerspective;

    private double x;
    private double y;
    private double z;
    private float yaw;
    private float pitch;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastFrameTime;
    private long dontMoveFreeCamBefore;

    private boolean cameraLock;
    private boolean eyeLock;
    private boolean followCamera;
    private double followDeltaX;
    private double followDeltaY;
    private double followDeltaZ;

    private boolean picking;
    private boolean cameraRestoredForPicking;
    private boolean entitiesRendering;
    private int perspectiveBeforeEntityRendering;

    private Entity overriddenEntity;
    private double entityX;
    private double entityY;
    private double entityZ;
    private double entityLastX;
    private double entityLastY;
    private double entityLastZ;
    private double entityPreviousX;
    private double entityPreviousY;
    private double entityPreviousZ;
    private float entityYaw;
    private float entityPitch;
    private float entityPreviousYaw;
    private float entityPreviousPitch;
    private boolean entityNoClip;

    private boolean handStateOverridden;
    private float handRenderYaw;
    private float handRenderPitch;
    private float handPreviousRenderYaw;
    private float handPreviousRenderPitch;

    private FreeCam() {}

    public boolean isActive() {
        return active;
    }

    public FreeCamConfig getConfig() {
        return ConfigRepository.INSTANCE.getConfig();
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

    public float getYaw() {
        return yaw;
    }

    public float getPitch() {
        return pitch;
    }

    public void toggle() {
        if (active) {
            disable();
        } else {
            enable();
        }
    }

    public void onKeyInput() {
        if (minecraft.thePlayer == null || minecraft.currentScreen != null) {
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

    public boolean onMouseTurn(Entity entity, float yawDelta, float pitchDelta) {
        if (!active || entity != player) {
            return false;
        }

        if (cameraLock || followCamera) {
            return false;
        }

        if (!eyeLock) {
            yaw += yawDelta * 0.15F;
            pitch -= pitchDelta * 0.15F;
            pitch = MathHelper.clamp_float(pitch, -90.0F, 90.0F);
            calculateVectors();
        }

        return true;
    }

    public void onClientTickStart() {
        if (!active) {
            return;
        }

        if (minecraft.theWorld == null || minecraft.thePlayer != player ||
                minecraft.renderViewEntity != cameraEntity || playerInput == null) {
            disable();
            return;
        }

        while (minecraft.gameSettings.keyBindTogglePerspective.isPressed()) {
            // Freecam owns the perspective until it is disabled.
        }

        MovementInput expectedInput = cameraLock || followCamera ? playerInput : freeCamPlayerInput;
        if (player.movementInput != expectedInput) {
            player.movementInput = expectedInput;
        }

        playerInput.updatePlayerMoveState();
    }

    public boolean shouldShowMyName() {
        return active && getConfig().showMyName;
    }

    public boolean shouldRenderTarget() {
        return !active || !cameraLock && !eyeLock && !followCamera && getConfig().target;
    }

    public boolean shouldRenderHands() {
        return active && !cameraLock && !eyeLock && !followCamera && getConfig().renderHands;
    }

    public void onBeforePick() {
        picking = true;
        cameraRestoredForPicking = false;

        if (overriddenEntity != null) {
            restoreEntityPosition();
            cameraRestoredForPicking = true;
        }
    }

    public void onAfterPick() {
        if (cameraRestoredForPicking && overriddenEntity != null) {
            moveEntityToCameraPosition();
        }

        cameraRestoredForPicking = false;
        picking = false;
    }

    public boolean shouldOverrideCameraEntityForPicking(Entity entity) {
        return active && !cameraLock && !eyeLock && !followCamera && getConfig().target &&
                picking && entity == cameraEntity;
    }

    public Vec3 getTargetPosition() {
        return Vec3.createVectorHelper(x, y, z);
    }

    public Vec3 getTargetLookVector() {
        float yawCos = MathHelper.cos(-yaw * 0.017453292F - (float)Math.PI);
        float yawSin = MathHelper.sin(-yaw * 0.017453292F - (float)Math.PI);
        float pitchCos = -MathHelper.cos(-pitch * 0.017453292F);
        float pitchSin = MathHelper.sin(-pitch * 0.017453292F);
        return Vec3.createVectorHelper(yawSin * pitchCos, pitchSin, yawCos * pitchCos);
    }

    public AxisAlignedBB getTargetSearchBox(Entity entity, AxisAlignedBB box) {
        if (!shouldOverrideCameraEntityForPicking(entity)) {
            return box;
        }

        return box.getOffsetBoundingBox(x - entity.posX, y - entity.posY, z - entity.posZ);
    }

    public void onRenderTickStart(float partialTicks) {
        if (!active) {
            return;
        }

        long currentTime = System.nanoTime();
        double frameTime = Math.min((currentTime - lastFrameTime) / 1.0E9D, MAX_FRAME_TIME_SECONDS);
        lastFrameTime = currentTime;
        if (frameTime <= 0.0D) {
            return;
        }

        if (followCamera) {
            Vec3 position = cameraEntity.getPosition(partialTicks);
            x = position.xCoord + followDeltaX;
            y = position.yCoord + followDeltaY;
            z = position.zCoord + followDeltaZ;
        } else {
            updateCameraPosition(currentTime, frameTime);
        }

        applyEyeLock(partialTicks);
    }

    public void renderDebugInfo(FontRenderer fontRenderer) {
        if (!active || !minecraft.gameSettings.showDebugInfo) {
            return;
        }

        fontRenderer.drawStringWithShadow("FreeCam", 2, 124, 14737632);
        fontRenderer.drawStringWithShadow(
                String.format("XYZ: %.3f / %.5f / %.3f", x, y, z), 2, 134, 14737632);
        fontRenderer.drawStringWithShadow(
                String.format("Facing: (%.1f / %.1f)", MathHelper.wrapAngleTo180_float(yaw),
                        MathHelper.wrapAngleTo180_float(pitch)),
                2, 144, 14737632);
    }

    public void onBeforeRenderWorld() {
        if (!active || overriddenEntity != null || minecraft.renderViewEntity != cameraEntity) {
            return;
        }

        overriddenEntity = cameraEntity;
        saveEntityPosition();
        moveEntityToCameraPosition();
    }

    public void onAfterRenderWorld() {
        if (entitiesRendering) {
            minecraft.gameSettings.thirdPersonView = perspectiveBeforeEntityRendering;
            entitiesRendering = false;
        }
        restoreHandState();

        if (overriddenEntity != null) {
            restoreEntityPosition();
            overriddenEntity = null;
        }

        cameraRestoredForPicking = false;
        picking = false;
    }

    public void onBeforeRenderEntity(Entity entity) {
        if (overriddenEntity == entity) {
            restoreEntityPosition();
        }
    }

    public void onAfterRenderEntity(Entity entity) {
        if (overriddenEntity == entity) {
            moveEntityToCameraPosition();
        }
    }

    public void onBeforeRenderEntities() {
        entitiesRendering = true;
        perspectiveBeforeEntityRendering = minecraft.gameSettings.thirdPersonView;
        if (overriddenEntity != null) {
            minecraft.gameSettings.thirdPersonView = 1;
        }
    }

    public void onAfterRenderEntities() {
        if (!entitiesRendering) {
            return;
        }

        minecraft.gameSettings.thirdPersonView = perspectiveBeforeEntityRendering;
        entitiesRendering = false;
    }

    public void onBeforeRenderHands() {
        if (!shouldRenderHands() || handStateOverridden || player == null) {
            return;
        }

        handStateOverridden = true;
        handRenderYaw = player.renderArmYaw;
        handRenderPitch = player.renderArmPitch;
        handPreviousRenderYaw = player.prevRenderArmYaw;
        handPreviousRenderPitch = player.prevRenderArmPitch;
        player.renderArmYaw = player.prevRenderArmYaw = yaw;
        player.renderArmPitch = player.prevRenderArmPitch = pitch;
    }

    public void onAfterRenderHands() {
        restoreHandState();
    }

    public double getNameDistanceSquared(Entity entity) {
        double dx = entity.posX - x;
        double dy = entity.posY - y;
        double dz = entity.posZ - z;
        return dx * dx + dy * dy + dz * dz;
    }

    private void enable() {
        EntityPlayerSP currentPlayer = minecraft.thePlayer;
        EntityLivingBase currentCameraEntity = minecraft.renderViewEntity;
        if (currentPlayer == null || minecraft.theWorld == null || currentCameraEntity == null) {
            return;
        }

        player = currentPlayer;
        cameraEntity = currentCameraEntity;
        playerInput = currentPlayer.movementInput;
        playerInput.updatePlayerMoveState();
        freeCamPlayerInput = createFreeCamPlayerInput(playerInput);
        previousPerspective = minecraft.gameSettings.thirdPersonView;

        x = currentCameraEntity.posX;
        y = currentCameraEntity.posY;
        z = currentCameraEntity.posZ;
        yaw = currentCameraEntity.rotationYaw;
        pitch = currentCameraEntity.rotationPitch;
        calculateVectors();

        double distance = -2.0D;
        x += forwards.x * distance;
        y += forwards.y * distance;
        z += forwards.z * distance;

        forwardVelocity = 0.0D;
        leftVelocity = 0.0D;
        upVelocity = 0.0D;
        lastFrameTime = System.nanoTime();
        dontMoveFreeCamBefore = getConfig().rememberInputState ?
                lastFrameTime + TimeUnit.MILLISECONDS.toNanos(REMEMBER_STATE_DELAY_MS) : 0L;

        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        currentPlayer.movementInput = freeCamPlayerInput;
        minecraft.gameSettings.thirdPersonView = 0;
        active = true;
    }

    private void disable() {
        if (!active) {
            return;
        }

        onAfterRenderWorld();
        active = false;

        if (player != null && (player.movementInput == freeCamPlayerInput || player.movementInput == playerInput)) {
            player.movementInput = playerInput;
        }
        minecraft.gameSettings.thirdPersonView = previousPerspective;

        player = null;
        cameraEntity = null;
        playerInput = null;
        freeCamPlayerInput = null;
        forwardVelocity = 0.0D;
        leftVelocity = 0.0D;
        upVelocity = 0.0D;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
    }

    private MovementInput createFreeCamPlayerInput(MovementInput source) {
        MovementInput input = new MovementInput();
        if (getConfig().rememberInputState) {
            input.moveForward = source.moveForward;
            input.moveStrafe = source.moveStrafe;
            input.jump = source.jump;
            input.sneak = source.sneak;
        }
        return input;
    }

    private void toggleCameraLock() {
        if (!active || followCamera) {
            return;
        }

        cameraLock = !cameraLock;
        player.movementInput = cameraLock ? playerInput : freeCamPlayerInput;
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
            cameraLock = false;
            eyeLock = false;
            player.movementInput = playerInput;
            Vec3 position = cameraEntity.getPosition(1.0F);
            followDeltaX = x - position.xCoord;
            followDeltaY = y - position.yCoord;
            followDeltaZ = z - position.zCoord;
        } else {
            player.movementInput = freeCamPlayerInput;
        }
    }

    private void updateCameraPosition(long currentTime, double frameTime) {
        FreeCamConfig config = getConfig();
        double forwardImpulse = cameraLock ? 0.0D : playerInput.moveForward;
        double leftImpulse = cameraLock ? 0.0D : playerInput.moveStrafe;
        double upImpulse = cameraLock ? 0.0D :
                (playerInput.jump ? 1.0D : 0.0D) - (playerInput.sneak ? 1.0D : 0.0D);
        double slowdown = Math.pow(config.slowdownFactor, frameTime);
        forwardVelocity = combineMovement(
                forwardVelocity, forwardImpulse, frameTime, config.acceleration, slowdown);
        leftVelocity = combineMovement(
                leftVelocity, leftImpulse, frameTime, config.acceleration, slowdown);
        upVelocity = combineMovement(
                upVelocity, upImpulse, frameTime, config.acceleration, slowdown);

        double dx = forwards.x * forwardVelocity + left.x * leftVelocity;
        double dy = forwards.y * forwardVelocity + upVelocity + left.y * leftVelocity;
        double dz = forwards.z * forwardVelocity + left.z * leftVelocity;
        double speed = Math.sqrt(dx * dx + dy * dy + dz * dz);
        if (speed > config.maxSpeed) {
            double factor = config.maxSpeed / speed;
            forwardVelocity *= factor;
            leftVelocity *= factor;
            upVelocity *= factor;
            dx *= factor;
            dy *= factor;
            dz *= factor;
        }

        if (!config.rememberInputState || currentTime >= dontMoveFreeCamBefore) {
            x += dx * frameTime;
            y += dy * frameTime;
            z += dz * frameTime;
        }
    }

    private void applyEyeLock(float partialTicks) {
        if (!eyeLock || cameraEntity == null) {
            return;
        }

        Vec3 position = cameraEntity.getPosition(partialTicks);
        double dx = x - position.xCoord;
        double dy = y - position.yCoord;
        double dz = z - position.zCoord;
        pitch = (float)(Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180.0D);
        yaw = (float)(Math.atan2(dz, dx) / Math.PI * 180.0D + 90.0D);
        pitch = MathHelper.clamp_float(pitch, -90.0F, 90.0F);
        calculateVectors();
    }

    private void calculateVectors() {
        rotation.set(0.0F, 0.0F, 0.0F, 1.0F);
        rotation.mul(Vector3f.YP.rotationDegrees(-yaw));
        if (!getConfig().spectatorMovement) {
            rotation.mul(Vector3f.XP.rotationDegrees(pitch));
        }

        forwards.set(0.0F, 0.0F, 1.0F);
        forwards.transform(rotation);
        up.set(0.0F, 1.0F, 0.0F);
        up.transform(rotation);
        left.set(1.0F, 0.0F, 0.0F);
        left.transform(rotation);
    }

    private double combineMovement(double velocity, double impulse, double frameTime, double acceleration, double slowdown) {
        if (impulse > 0.0D) {
            if (velocity < 0.0D) {
                velocity = 0.0D;
            }
            return velocity + acceleration * impulse * frameTime;
        }
        if (impulse < 0.0D) {
            if (velocity > 0.0D) {
                velocity = 0.0D;
            }
            return velocity + acceleration * impulse * frameTime;
        }
        return velocity * slowdown;
    }

    private void saveEntityPosition() {
        entityX = overriddenEntity.posX;
        entityY = overriddenEntity.posY;
        entityZ = overriddenEntity.posZ;
        entityLastX = overriddenEntity.lastTickPosX;
        entityLastY = overriddenEntity.lastTickPosY;
        entityLastZ = overriddenEntity.lastTickPosZ;
        entityPreviousX = overriddenEntity.prevPosX;
        entityPreviousY = overriddenEntity.prevPosY;
        entityPreviousZ = overriddenEntity.prevPosZ;
        entityYaw = overriddenEntity.rotationYaw;
        entityPitch = overriddenEntity.rotationPitch;
        entityPreviousYaw = overriddenEntity.prevRotationYaw;
        entityPreviousPitch = overriddenEntity.prevRotationPitch;
        entityNoClip = overriddenEntity.noClip;
    }

    private void restoreEntityPosition() {
        overriddenEntity.posX = entityX;
        overriddenEntity.posY = entityY;
        overriddenEntity.posZ = entityZ;
        overriddenEntity.lastTickPosX = entityLastX;
        overriddenEntity.lastTickPosY = entityLastY;
        overriddenEntity.lastTickPosZ = entityLastZ;
        overriddenEntity.prevPosX = entityPreviousX;
        overriddenEntity.prevPosY = entityPreviousY;
        overriddenEntity.prevPosZ = entityPreviousZ;
        overriddenEntity.rotationYaw = entityYaw;
        overriddenEntity.rotationPitch = entityPitch;
        overriddenEntity.prevRotationYaw = entityPreviousYaw;
        overriddenEntity.prevRotationPitch = entityPreviousPitch;
        overriddenEntity.noClip = entityNoClip;
    }

    private void moveEntityToCameraPosition() {
        overriddenEntity.posX = overriddenEntity.lastTickPosX = overriddenEntity.prevPosX = x;
        overriddenEntity.posY = overriddenEntity.lastTickPosY = overriddenEntity.prevPosY = y;
        overriddenEntity.posZ = overriddenEntity.lastTickPosZ = overriddenEntity.prevPosZ = z;
        overriddenEntity.rotationYaw = overriddenEntity.prevRotationYaw = yaw;
        overriddenEntity.rotationPitch = overriddenEntity.prevRotationPitch = pitch;
        overriddenEntity.noClip = true;
    }

    private void restoreHandState() {
        if (!handStateOverridden || player == null) {
            return;
        }

        player.renderArmYaw = handRenderYaw;
        player.renderArmPitch = handRenderPitch;
        player.prevRenderArmYaw = handPreviousRenderYaw;
        player.prevRenderArmPitch = handPreviousRenderPitch;
        handStateOverridden = false;
    }
}