package com.zergatul.freecam;

import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraft.block.BlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.settings.PointOfView;
import net.minecraft.entity.Entity;
import net.minecraft.state.Property;
import net.minecraft.util.MovementInput;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.Util;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.vector.Quaternion;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.*;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class FreeCam {

    public static final FreeCam INSTANCE = new FreeCam();

    private static final int REMEMBER_STATE_DELAY_MS = 400;

    private final Minecraft mc = Minecraft.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamConfig config = ConfigStore.INSTANCE.load();
    private boolean active;
    private PointOfView oldCameraType;
    private MovementInput playerInput;
    private MovementInput freecamInput;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private long dontMoveFreeCamBefore;
    private boolean insideRenderDebug;
    private boolean cameraLock;
    private boolean eyeLock;
    private boolean followCamera;
    private double followDeltaX, followDeltaY, followDeltaZ;
    private int openSettingsScreenTicks = -1;

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

    public void toggleCameraLock() {
        if (active && !followCamera) {
            assert mc.player != null;

            cameraLock = !cameraLock;
            if (cameraLock) {
                mc.player.input = playerInput;
            } else {
                mc.player.input = freecamInput;
            }
        }
    }

    public void toggleEyeLock() {
        if (active && !followCamera) {
            eyeLock = !eyeLock;
        }
    }

    public void toggleFollowCamera() {
        if (!active) {
            return;
        }

        assert mc.player != null;

        if (!followCamera) {
            Entity entity = mc.getCameraEntity();
            if (entity == null) {
                return;
            }

            followCamera = true;
            mc.player.input = playerInput;
            cameraLock = false;
            eyeLock = false;

            Vector3d position = entity.getEyePosition(mc.getFrameTime());
            followDeltaX = x - position.x;
            followDeltaY = y - position.y;
            followDeltaZ = z - position.z;
        } else {
            followCamera = false;
            mc.player.input = freecamInput;
        }
    }

    public void enable() {
        if (active) {
            return;
        }

        Entity entity = mc.getCameraEntity();
        if (entity == null) {
            return;
        }

        assert mc.player != null;

        active = true;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        oldCameraType = mc.options.getCameraType();
        playerInput = mc.player.input;
        playerInput.tick(false);
        mc.player.input = freecamInput = createFreeCamInput(playerInput);
        mc.options.setCameraType(PointOfView.THIRD_PERSON_BACK);
        if (oldCameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
            mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
        }

        if (config.rememberInputState) {
            dontMoveFreeCamBefore = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(REMEMBER_STATE_DELAY_MS);
        }

        float frameTime = mc.getFrameTime();
        x = MathHelper.lerp(frameTime, entity.xo, entity.getX());
        y = MathHelper.lerp(frameTime, entity.yo, entity.getY()) + entity.getEyeHeight();
        z = MathHelper.lerp(frameTime, entity.zo, entity.getZ());
        yRot = entity.getViewYRot(frameTime);
        xRot = entity.getViewXRot(frameTime);

        calculateVectors();

        double distance = -2;
        x += (double)this.forwards.x() * distance;
        y += (double)this.forwards.y() * distance;
        z += (double)this.forwards.z() * distance;

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
        followCamera = false;
        PointOfView cameraType = mc.options.getCameraType();
        mc.options.setCameraType(oldCameraType);
        mc.player.input = playerInput;
        if (cameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
            mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
        }
        oldCameraType = null;
    }

    public void onKeyInput() {
        if (mc.player == null) {
            return;
        }
        if (mc.screen != null) {
            return;
        }
        while (KeyBindings.TOGGLE_FREE_CAM.consumeClick()) {
            toggle();
        }
        while (KeyBindings.TOGGLE_CAMERA_LOCK.consumeClick()) {
            toggleCameraLock();
        }
        while (KeyBindings.TOGGLE_EYE_LOCK.consumeClick()) {
            toggleEyeLock();
        }
        while (KeyBindings.TOGGLE_FOLLOW_CAM.consumeClick()) {
            toggleFollowCamera();
        }
    }

    public boolean onClientChat(String message) {
        if (message == null || !".freecam".equalsIgnoreCase(message.trim())) {
            return false;
        }

        mc.gui.getChat().addRecentChat(message);

        openSettingsScreenTicks = 4;
        return true;
    }

    public void onMouseTurn(double yRot, double xRot) {
        if (!eyeLock) {
            this.xRot += (float) xRot * 0.15F;
            this.yRot += (float) yRot * 0.15F;
            this.xRot = MathHelper.clamp(this.xRot, -90, 90);
            calculateVectors();
        }
    }

    public void onRenderTickStart() {
        if (active) {
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
                    Vector3d position = entity.getEyePosition(mc.getFrameTime());
                    x = position.x + followDeltaX;
                    y = position.y + followDeltaY;
                    z = position.z + followDeltaZ;
                }
            } else {
                MovementInput input = playerInput;
                float forwardImpulse = !cameraLock ? (input.up ? 1 : 0) + (input.down ? -1 : 0) : 0;
                float leftImpulse = !cameraLock ? (input.left ? 1 : 0) + (input.right ? -1 : 0) : 0;
                float upImpulse = !cameraLock ? ((input.jumping ? 1 : 0) + (input.shiftKeyDown ? -1 : 0)) : 0;
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
                double speed = new Vector3d(dx, dy, dz).length() / frameTime;
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

            applyEyeLock();
        }
    }

    public void onClientTickStart() {
        if (active) {
            while (mc.options.keyTogglePerspective.consumeClick()) {
                // consume clicks
            }
            if (mc.player != null && mc.player.input != playerInput) {
                playerInput.tick(false);
            }
        }

        if (openSettingsScreenTicks > 0 && --openSettingsScreenTicks == 0) {
            openSettingsScreenTicks = -1;
            mc.setScreen(new FreeCamSettingsScreen());
        }
    }

    public void onWorldUnload() {
        disable();
    }

    public boolean shouldRedirectMouseTurn() {
        return active && !cameraLock && !followCamera;
    }

    public boolean shouldOverrideCameraEntityPosition(Entity entity) {
        return active && !cameraLock && !eyeLock && !followCamera && config.target &&
                entity == mc.getCameraEntity() && (MixinGameRendererHelper.insidePick || insideRenderDebug);
    }

    public AxisAlignedBB getTargetSearchBox(Entity entity, AxisAlignedBB box) {
        if (!shouldOverrideCameraEntityPosition(entity)) {
            return box;
        }

        return box.move(
                x - entity.getX(),
                y - entity.getY() - entity.getEyeHeight(),
                z - entity.getZ());
    }

    public void onRenderDebugScreenLeft(List<String> list) {
        if (active) {
            list.add("");
            String coordinates = String.format(Locale.ROOT, "Free Cam XYZ: %.3f / %.5f / %.3f", x, y, z);
            list.add(coordinates);
        }
    }

    public void onRenderDebugScreenRight(List<String> list) {
        if (!active) {
            return;
        }
        if (cameraLock || eyeLock || followCamera || !config.target) {
            return;
        }

        assert mc.level != null;
        assert mc.player != null;

        insideRenderDebug = true;
        try {
            RayTraceResult hit = mc.player.pick(20.0D, 0.0F, false);
            if (hit.getType() == RayTraceResult.Type.BLOCK) {
                BlockPos pos = ((BlockRayTraceResult)hit).getBlockPos();
                BlockState state = mc.level.getBlockState(pos);
                list.add("");
                list.add(TextFormatting.UNDERLINE + "Free Cam Targeted Block: " + pos.getX() + ", " + pos.getY() + ", " + pos.getZ());
                list.add(String.valueOf(Registry.BLOCK.getKey(state.getBlock())));

                for(Map.Entry<Property<?>, Comparable<?>> entry : state.getValues().entrySet()) {
                    list.add(this.getPropertyValueString(entry));
                }

                for(ResourceLocation resourcelocation : state.getBlock().getTags()) {
                    list.add("#" + resourcelocation);
                }
            }
        }
        finally {
            insideRenderDebug = false;
        }
    }

    public boolean shouldRenderHands() {
        return config.renderHands && !cameraLock && !eyeLock && !followCamera;
    }

    public boolean shouldRenderCrosshair() {
        return active && !cameraLock && !eyeLock && !followCamera && config.target;
    }

    public boolean shouldShowMyName() {
        return active && config.showMyName;
    }

    private void applyEyeLock() {
        if (eyeLock) {
            float frameTime = mc.getFrameTime();
            Entity entity = mc.getCameraEntity();
            if (entity == null) {
                return;
            }

            double xe = MathHelper.lerp(frameTime, entity.xo, entity.getX());
            double ye = MathHelper.lerp(frameTime, entity.yo, entity.getY()) + entity.getEyeHeight();
            double ze = MathHelper.lerp(frameTime, entity.zo, entity.getZ());
            double dx = x - xe;
            double dy = y - ye;
            double dz = z - ze;
            this.xRot = (float) (Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180);
            this.yRot = (float) (Math.atan2(dz, dx) / Math.PI * 180 + 90);
            this.xRot = MathHelper.clamp(this.xRot, -90, 90);
            calculateVectors();
        }
    }

    private String getPropertyValueString(Map.Entry<Property<?>, Comparable<?>> p_211534_1_) {
        Property<?> property = p_211534_1_.getKey();
        Comparable<?> comparable = p_211534_1_.getValue();
        String s = Util.getPropertyName(property, comparable);
        if (Boolean.TRUE.equals(comparable)) {
            s = TextFormatting.GREEN + s;
        } else if (Boolean.FALSE.equals(comparable)) {
            s = TextFormatting.RED + s;
        }

        return property.getName() + ": " + s;
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

    private MovementInput createFreeCamInput(MovementInput input) {
        MovementInput result = new MovementInput();
        if (config.rememberInputState) {
            result.leftImpulse = input.leftImpulse;
            result.forwardImpulse = input.forwardImpulse;
            result.up = input.up;
            result.down = input.down;
            result.left = input.left;
            result.right = input.right;
            result.jumping = input.jumping;
            result.shiftKeyDown = input.shiftKeyDown;
        }
        return result;
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
}