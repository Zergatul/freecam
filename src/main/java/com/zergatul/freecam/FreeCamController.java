package com.zergatul.freecam;

import com.mojang.math.Quaternion;
import com.mojang.math.Vector3f;
import net.minecraft.ChatFormatting;
import net.minecraft.Util;
import net.minecraft.client.CameraType;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.minecraft.client.Options;
import net.minecraft.client.player.Input;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.*;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.Property;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.*;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private final Minecraft mc = Minecraft.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private final FreeCamConfig config = ConfigStore.instance.load();
    private final MutableComponent chatPrefix = new TextComponent("[freecam]").withStyle(ChatFormatting.GREEN).append(" ");
    private final ChatType chatType = ChatType.SYSTEM;
    private boolean active;
    private CameraType oldCameraType;
    private Input playerInput;
    private Input freecamInput;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private boolean freecamHitResultPicking;
    private boolean cameraLock;
    private boolean eyeLock;
    private boolean followCamera;
    private double followDeltaX, followDeltaY, followDeltaZ;
    private boolean gameRendererPicking;

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

    public void toggleCameraLock() {
        assert mc.player != null;

        if (active && !followCamera) {
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
                mc.player.input = freecamInput;
            }
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

        active = true;
        cameraLock = false;
        eyeLock = false;
        followCamera = false;
        oldCameraType = mc.options.getCameraType();
        playerInput = mc.player.input;
        mc.player.input = freecamInput = new Input();
        mc.options.setCameraType(CameraType.THIRD_PERSON_BACK);
        if (oldCameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
            mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
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
        x += (double)this.forwards.x() * distance;
        y += (double)this.forwards.y() * distance;
        z += (double)this.forwards.z() * distance;

        forwardVelocity = 0;
        leftVelocity = 0;
        upVelocity = 0;
        lastTime = 0;
    }

    public void disable() {
        assert mc.player != null;

        if (!active) {
            return;
        }

        active = false;
        CameraType cameraType = mc.options.getCameraType();
        mc.options.setCameraType(oldCameraType);
        mc.player.input = playerInput;
        if (cameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
            mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
        }
        oldCameraType = null;
    }

    public void onHandleKeyBindings() {
        if (mc.player == null) {
            return;
        }
        if (mc.screen != null) {
            return;
        }
        while (KeyBindingsController.toggleFreeCam.consumeClick()) {
            toggle();
        }
        while (KeyBindingsController.toggleCameraLock.consumeClick()) {
            toggleCameraLock();
        }
        while (KeyBindingsController.toggleEyeLock.consumeClick()) {
            toggleEyeLock();
        }
        while (KeyBindingsController.toggleFollowCam.consumeClick()) {
            toggleFollowCamera();
        }
    }

    public void onClientChat(String message, ModApiWrapper.Event event) {
        if (message == null) {
            return;
        }
        message = message.toLowerCase(Locale.ROOT);
        if (message.startsWith(".freecam")) {
            String[] parts = message.split("\s+");
            switch (parts.length) {
                case 1:
                    if (parts[0].equals(".freecam")) {
                        mc.gui.handleChat(chatType, chatPrefix.copy()
                                .append(new TextComponent("Current settings").withStyle(ChatFormatting.YELLOW)).append("\n")
                                .append(new TextComponent("- maxspeed=" + config.maxSpeed).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(new TextComponent("- acceleration=" + config.acceleration).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(new TextComponent("- slowdown=" + config.slowdownFactor).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(new TextComponent("- hands=" + (config.renderHands ? 1 : 0)).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(new TextComponent("- target=" + (config.target ? 1 : 0)).withStyle(ChatFormatting.WHITE)),
                                Util.NIL_UUID);
                    } else {
                        printHelp();
                    }
                    break;

                case 3:
                    double value;
                    try {
                        value = Double.parseDouble(parts[2]);
                    } catch (NumberFormatException e) {
                        value = Double.NaN;
                    }
                    if (Double.isNaN(value)) {
                        printError("Cannot parse value");
                    } else {
                        switch (parts[1]) {
                            case "maxspeed":
                            case "max":
                            case "speed":
                            case "s":
                                if (value < FreeCamConfig.MinMaxSpeed || value > FreeCamConfig.MaxMaxSpeed) {
                                    printError("Value out of range. Allowed range: [" + FreeCamConfig.MinMaxSpeed + " - " + FreeCamConfig.MaxMaxSpeed + "]");
                                } else {
                                    config.maxSpeed = value;
                                    saveConfig();
                                }
                                break;

                            case "acceleration":
                            case "acc":
                            case "a":
                                if (value < FreeCamConfig.MinAcceleration || value > FreeCamConfig.MaxAcceleration) {
                                    printError("Value out of range. Allowed range: [" + FreeCamConfig.MinAcceleration + " - " + FreeCamConfig.MaxAcceleration + "]");
                                } else {
                                    config.acceleration = value;
                                    saveConfig();
                                }
                                break;

                            case "slowdown":
                            case "slow":
                            case "sd":
                                if (value < FreeCamConfig.MinSlowdownFactor || value > FreeCamConfig.MaxSlowdownFactor) {
                                    printError("Value out of range. Allowed range: [" + FreeCamConfig.MinSlowdownFactor + " - " + FreeCamConfig.MinSlowdownFactor + "]");
                                } else {
                                    config.slowdownFactor = value;
                                    saveConfig();
                                }
                                break;

                            case "hands":
                                if (value == 0) {
                                    config.renderHands = false;
                                    saveConfig();
                                } else if (value == 1) {
                                    config.renderHands = true;
                                    saveConfig();
                                } else {
                                    printError("Invalid value. Only 0 or 1 accepted.");
                                }
                                break;

                            case "target":
                                if (value == 0) {
                                    config.target = false;
                                    saveConfig();
                                } else if (value == 1) {
                                    config.target = true;
                                    saveConfig();
                                } else {
                                    printError("Invalid value. Only 0 or 1 accepted.");
                                }
                                break;

                            default:
                                printHelp();
                                break;
                        }
                    }
                    break;

                default:
                    printHelp();
                    break;

            }
            event.cancel();
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
    }

    public void onWorldUnload() {
        disable();
    }

    public boolean shouldOverrideCameraEntityPosition(Entity entity) {
        if (active && !cameraLock && !eyeLock && !followCamera && config.target) {
            return entity == mc.getCameraEntity() && gameRendererPicking || freecamHitResultPicking;
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
        if (!active) {
            return;
        }
        if (cameraLock || eyeLock || followCamera) {
            return;
        }
        if (!config.target) {
            return;
        }

        freecamHitResultPicking = true;
        try {
            HitResult hit = mc.player.pick(20.0D, 0.0F, false);
            if (hit.getType() == HitResult.Type.BLOCK) {
                BlockPos pos = ((BlockHitResult)hit).getBlockPos();
                BlockState state = mc.level.getBlockState(pos);
                list.add("");
                list.add(ChatFormatting.UNDERLINE + "Free Cam Targeted Block: " + pos.getX() + ", " + pos.getY() + ", " + pos.getZ());
                list.add(String.valueOf(ForgeRegistries.BLOCKS.getKey(state.getBlock())));

                for (var entry: state.getValues().entrySet()) {
                    list.add(getPropertyValueString(entry));
                }

                state.getTags().map(tag -> "#" + tag.location()).forEach(list::add);
            }
        }
        finally {
            freecamHitResultPicking = false;
        }
    }

    public void onBeforeGameRendererPick() {
        gameRendererPicking = true;
    }

    public void onAfterGameRendererPick() {
        gameRendererPicking = false;
    }

    public boolean getBobView(Options options) {
        if (active) {
            return false;
        } else {
            return options.bobView;
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

    private void saveConfig() {
        ConfigStore.instance.save(config);
        printInfo("Config updated");
    }

    private void disableKey(KeyMapping key) {
        while (key.consumeClick()) {}
        key.setDown(false);
    }

    private void printInfo(String message) {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new TextComponent(message).withStyle(ChatFormatting.GOLD)),
                Util.NIL_UUID);
    }

    private void printError(String message) {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new TextComponent(message).withStyle(ChatFormatting.RED)),
                Util.NIL_UUID);
    }

    private void printHelp() {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new TextComponent("Invalid syntax").withStyle(ChatFormatting.RED)).append("\n")
                .append(new TextComponent("- ").withStyle(ChatFormatting.WHITE))
                        .append(new TextComponent(".freecam maxspeed 50").withStyle(ChatFormatting.YELLOW))
                        .append(new TextComponent(" set maximum speed, blocks/second").withStyle(ChatFormatting.WHITE))
                        .append("\n")
                        .append(new TextComponent(" (synonyms: max, speed, s)").withStyle(ChatFormatting.AQUA))
                        .append("\n")
                .append(new TextComponent("- ").withStyle(ChatFormatting.WHITE))
                    .append(new TextComponent(".freecam acceleration 50").withStyle(ChatFormatting.YELLOW))
                    .append(new TextComponent(" set acceleration speed, blocks/second^2").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                    .append(new TextComponent(" (synonyms: acc, a)").withStyle(ChatFormatting.AQUA))
                    .append("\n")
                .append(new TextComponent("- ").withStyle(ChatFormatting.WHITE))
                    .append(new TextComponent(".freecam slowdown 0.01").withStyle(ChatFormatting.YELLOW))
                    .append(new TextComponent(" set slow down speed. When no keys is pressed speed is multiplied by this value every second.").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                    .append(new TextComponent(" (synonyms: slow, sd)").withStyle(ChatFormatting.AQUA))
                    .append("\n")
                .append(new TextComponent("- ").withStyle(ChatFormatting.WHITE))
                    .append(new TextComponent(".freecam hands 1").withStyle(ChatFormatting.YELLOW))
                    .append(new TextComponent(" render hands while in freecam. Values: 0/1.").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                .append(new TextComponent("- ").withStyle(ChatFormatting.WHITE))
                    .append(new TextComponent(".freecam target 0").withStyle(ChatFormatting.YELLOW))
                    .append(new TextComponent(" disable custom targeting in freecam. Values: 0/1.").withStyle(ChatFormatting.WHITE)),
                Util.NIL_UUID);
    }
}