package com.zergatul.freecam;

import com.mojang.math.Quaternion;
import com.mojang.math.Vector3f;
import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraft.ChatFormatting;
import net.minecraft.Util;
import net.minecraft.client.CameraType;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.minecraft.client.Options;
import net.minecraft.client.player.Input;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class FreeCam {

    public static final FreeCam INSTANCE = new FreeCam();

    private final static int REMEMBER_STATE_DELAY_MS = 400;

    private final Minecraft mc = Minecraft.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamConfig config = ConfigRepository.INSTANCE.load();
    private boolean active;
    private CameraType oldCameraType;
    private Input playerInput;
    private Input freeCamInput;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private boolean freeCamHitResultPicking;
    private boolean cameraLock;
    private boolean eyeLock;
    private boolean followCamera;
    private double followDeltaX, followDeltaY, followDeltaZ;
    private boolean gameRendererPicking;
    private boolean renderingPlayerInInventory;
    private long dontMoveFreeCamBefore;
    private int openSettingsScreenTicks = -1;

    private FreeCam() {}

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

    public void toggle() {
        if (active) {
            disable();
        } else {
            enable();
        }
    }

    public void toggleCameraLock() {
        assert mc.player != null;

        if (active && !followCamera) {
            cameraLock = !cameraLock;
            if (cameraLock) {
                // instantly stop the camera
                forwardVelocity = 0;
                leftVelocity = 0;
                upVelocity = 0;
                mc.player.input = playerInput;
            } else {
                mc.player.input = freeCamInput;
            }
        }
    }

    public void toggleEyeLock() {
        if (active && !followCamera) {
            eyeLock = !eyeLock;
        }
    }

    public void toggleFollowCamera() {
        assert mc.player != null;

        if (active) {
            followCamera = !followCamera;
            if (followCamera) {
                mc.player.input = playerInput;
                cameraLock = false;
                eyeLock = false;

                Entity entity = mc.getCameraEntity();
                if (entity == null) {
                    return;
                }

                Vec3 pos = entity.getEyePosition();
                followDeltaX = x - pos.x;
                followDeltaY = y - pos.y;
                followDeltaZ = z - pos.z;
            } else {
                mc.player.input = freeCamInput;
            }
        }
    }

    public void enable() {
        if (active) {
            return;
        }

        Entity entity = mc.getCameraEntity();
        if (mc.player == null || entity == null) {
            return;
        }

        active = true;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        playerInput = mc.player.input;
        mc.player.input = freeCamInput = createFreeCamInput(playerInput);
        switchCameraType(CameraType.THIRD_PERSON_BACK);

        if (config.rememberInputState) {
            dontMoveFreeCamBefore = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(REMEMBER_STATE_DELAY_MS);
        }

        float frameTime = mc.getFrameTime();
        Vec3 pos = entity.getEyePosition(frameTime);
        x = pos.x;
        y = pos.y;
        z = pos.z;
        yRot = entity.getViewYRot(frameTime);
        xRot = entity.getViewXRot(frameTime);

        calculateVectors();

        double distance = -2;
        x += this.forwards.x() * distance;
        y += this.forwards.y() * distance;
        z += this.forwards.z() * distance;

        forwardVelocity = 0;
        leftVelocity = 0;
        upVelocity = 0;
        lastTime = 0;
    }

    public void disable() {
        if (!active) {
            return;
        }

        assert mc.player != null;

        active = false;
        mc.player.input = playerInput;
        switchCameraType(oldCameraType);
    }

    public void onHandleKeyBindings() {
        if (mc.player == null) {
            return;
        }
        if (mc.screen != null) {
            return;
        }
        while (KeyBindings.toggleFreeCam.consumeClick()) {
            toggle();
        }
        while (KeyBindings.toggleCameraLock.consumeClick()) {
            toggleCameraLock();
        }
        while (KeyBindings.toggleEyeLock.consumeClick()) {
            toggleEyeLock();
        }
        while (KeyBindings.toggleFollowCam.consumeClick()) {
            toggleFollowCamera();
        }
    }

    public void onPlayerTurn(LocalPlayer player, double yRot, double xRot) {
        if (active && !cameraLock && !followCamera) {
            if (!eyeLock) {
                this.xRot += (float) xRot * 0.15F;
                this.yRot += (float) yRot * 0.15F;
                this.xRot = Mth.clamp(this.xRot, -90, 90);
                calculateVectors();
            }
        } else {
            player.turn(yRot, xRot);
        }
    }

    public boolean onRenderCrosshairIsFirstPerson(CameraType cameraType) {
        if (active && !cameraLock && !eyeLock && !followCamera && config.target) {
            return true;
        } else {
            return cameraType.isFirstPerson();
        }
    }

    public boolean onRenderItemInHandIsFirstPerson(CameraType cameraType) {
        if (active && config.renderHands && !cameraLock && !eyeLock && !followCamera) {
            return true;
        } else {
            return cameraType.isFirstPerson();
        }
    }

    public boolean onClientChat(String message) {
        if (message == null) {
            return false;
        }

        message = message.toLowerCase(Locale.ROOT);
        if (!message.startsWith(".freecam")) {
            return false;
        }

        mc.gui.getChat().addRecentChat(message);

        openSettingsScreenTicks = 4;
        return true;
    }

    public boolean shouldShowMyName() {
        return active && config.showMyName && !renderingPlayerInInventory;
    }

    public void onRenderTickStart(float partialTicks) {
        if (!active) {
            return;
        }

        if (lastTime == 0) {
            lastTime = System.nanoTime();
            return;
        }

        long currTime = System.nanoTime();
        float frameTime = (currTime - lastTime) / 1e9f;
        lastTime = currTime;

        if (followCamera) {
            Entity entity = mc.getCameraEntity();
            if (entity != null) {
                Vec3 pos = entity.getEyePosition(partialTicks);
                x = pos.x + followDeltaX;
                y = pos.y + followDeltaY;
                z = pos.z + followDeltaZ;
            }
        } else {
            Input input = playerInput;
            boolean handleKeys = !cameraLock && (!config.rememberInputState || dontMoveFreeCamBefore < currTime);
            float forwardImpulse = handleKeys ? (input.up ? 1 : 0) + (input.down ? -1 : 0) : 0;
            float leftImpulse = handleKeys ? (input.left ? 1 : 0) + (input.right ? -1 : 0) : 0;
            float upImpulse = handleKeys ? ((input.jumping ? 1 : 0) + (input.shiftKeyDown ? -1 : 0)) : 0;
            double slowdown = Math.pow(config.slowdownFactor, frameTime);
            forwardVelocity = combineMovement(forwardVelocity, forwardImpulse, frameTime, config.acceleration, slowdown);
            leftVelocity = combineMovement(leftVelocity, leftImpulse, frameTime, config.acceleration, slowdown);
            upVelocity = combineMovement(upVelocity, upImpulse, frameTime, config.acceleration, slowdown);

            double dx = (double) this.forwards.x() * forwardVelocity + (double) this.left.x() * leftVelocity;
            double dy = (double) this.forwards.y() * forwardVelocity + upVelocity + (double) this.left.y() * leftVelocity;
            double dz = (double) this.forwards.z() * forwardVelocity + (double) this.left.z() * leftVelocity;
            dx *= frameTime;
            dy *= frameTime;
            dz *= frameTime;
            double speed = new Vec3(dx, dy, dz).length() / frameTime;
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

        applyEyeLock(partialTicks);
    }

    public void onClientTickStart() {
        if (active) {
            disableKey(mc.options.keyTogglePerspective);
            playerInput.tick(false);
        }

        if (openSettingsScreenTicks > 0 && --openSettingsScreenTicks == 0) {
            openSettingsScreenTicks = -1;
            mc.setScreen(new FreeCamSettingsScreen());
        }
    }

    public void onWorldUnload() {
        disable();
    }

    public boolean shouldOverrideCameraEntityPosition(Entity entity) {
        if (active && !cameraLock && !eyeLock && !followCamera && config.target) {
            return entity == mc.getCameraEntity() && gameRendererPicking || freeCamHitResultPicking;
        } else {
            return false;
        }
    }

    public void onRenderDebugScreenLeft(List<String> list) {
        if (active) {
            list.add("");
            String coordinates = String.format(Locale.ROOT, "Free Cam XYZ: %.3f / %.5f / %.3f", x, y, z);
            list.add(coordinates);
        }
    }

    public void onRenderDebugScreenRight(List<String> list) {
        if (!active || mc.level == null || mc.player == null) {
            return;
        }
        if (cameraLock || eyeLock || followCamera) {
            return;
        }
        if (!config.target) {
            return;
        }

        freeCamHitResultPicking = true;
        try {
            HitResult hit = mc.player.pick(20.0D, 0.0F, false);
            if (hit.getType() == HitResult.Type.BLOCK) {
                BlockPos pos = ((BlockHitResult)hit).getBlockPos();
                BlockState state = mc.level.getBlockState(pos);
                list.add("");
                list.add(ChatFormatting.UNDERLINE + "Free Cam Targeted Block: " + pos.getX() + ", " + pos.getY() + ", " + pos.getZ());
                list.add(String.valueOf(Registry.BLOCK.getKey(state.getBlock())));

                for (var entry: state.getValues().entrySet()) {
                    list.add(getPropertyValueString(entry));
                }

                state.getTags().map(tag -> "#" + tag.location()).forEach(list::add);
            }
        } finally {
            freeCamHitResultPicking = false;
        }
    }

    public void onBeforeGameRendererPick() {
        gameRendererPicking = true;
    }

    public void onAfterGameRendererPick() {
        gameRendererPicking = false;
    }

    public void onBeforeRenderPlayerInInventory() {
        renderingPlayerInInventory = true;
    }

    public void onAfterRenderPlayerInInventory() {
        renderingPlayerInInventory = false;
    }

    public boolean getBobView(Options options) {
        if (active) {
            return false;
        } else {
            return options.bobView;
        }
    }

    private Input createFreeCamInput(Input playerInput) {
        if (config.rememberInputState) {
            Input result = new Input();
            result.up = playerInput.up;
            result.down = playerInput.down;
            result.left = playerInput.left;
            result.right = playerInput.right;
            result.jumping = playerInput.jumping;
            result.shiftKeyDown = playerInput.shiftKeyDown;
            result.leftImpulse = playerInput.leftImpulse;
            result.forwardImpulse = playerInput.forwardImpulse;
            return result;
        } else {
            return new Input();
        }
    }

    private void applyEyeLock(float partialTicks) {
        if (!eyeLock) {
            return;
        }

        Entity entity = mc.getCameraEntity();
        if (entity == null) {
            return;
        }

        Vec3 pos = entity.getEyePosition(partialTicks);
        double dx = x - pos.x;
        double dy = y - pos.y;
        double dz = z - pos.z;
        this.xRot = (float) (Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180);
        this.yRot = (float) (Math.atan2(dz, dx) / Math.PI * 180 + 90);
        this.xRot = Mth.clamp(this.xRot, -90, 90);
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

    private String getPropertyValueString(Map.Entry<Property<?>, Comparable<?>> p_94072_) {
        Property<?> property = p_94072_.getKey();
        Comparable<?> comparable = p_94072_.getValue();
        String s = Util.getPropertyName(property, comparable);
        if (Boolean.TRUE.equals(comparable)) {
            s = ChatFormatting.GREEN + s;
        } else if (Boolean.FALSE.equals(comparable)) {
            s = ChatFormatting.RED + s;
        }

        return property.getName() + ": " + s;
    }

    private void switchCameraType(CameraType type) {
        oldCameraType = mc.options.getCameraType();
        mc.options.setCameraType(type);
        if (oldCameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
            mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
        }
    }

    private void disableKey(KeyMapping key) {
        while (key.consumeClick()) {}
        key.setDown(false);
    }
}