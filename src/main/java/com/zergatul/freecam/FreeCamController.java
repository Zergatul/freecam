package com.zergatul.freecam;

import com.mojang.math.Quaternion;
import com.mojang.math.Vector3f;
import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import net.minecraft.ChatFormatting;
import net.minecraft.Util;
import net.minecraft.client.CameraType;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.Input;
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

import java.util.List;
import java.util.Locale;
import java.util.Map;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private final Minecraft mc = Minecraft.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private FreeCamConfig config = ConfigStore.instance.load();
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
    private boolean insideRenderDebug;
    private boolean cameraControlActive;
    private boolean eyeLock;
    private MutableComponent chatPrefix = Component.literal("[freecam]").withStyle(ChatFormatting.GREEN).append(" ");

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

    public void toggleCameraControl() {
        if (active) {
            cameraControlActive = !cameraControlActive;
            if (cameraControlActive) {
                mc.player.input = freecamInput;
            } else {
                mc.player.input = playerInput;
            }
        }
    }

    public void toggleEyeLock() {
        if (active) {
            eyeLock = !eyeLock;
        }
    }

    public void enable() {
        if (!active) {
            active = true;
            cameraControlActive = true;
            eyeLock = false;
            oldCameraType = mc.options.getCameraType();
            playerInput = mc.player.input;
            mc.player.input = freecamInput = new Input();
            mc.options.setCameraType(CameraType.THIRD_PERSON_BACK);
            if (oldCameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
                mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
            }

            float frameTime = mc.getFrameTime();
            Entity entity = mc.getCameraEntity();
            x = Mth.lerp(frameTime, entity.xo, entity.getX());
            y = Mth.lerp(frameTime, entity.yo, entity.getY()) + entity.getEyeHeight();
            z = Mth.lerp(frameTime, entity.zo, entity.getZ());
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
    }

    public void disable() {
        if (active) {
            active = false;
            CameraType cameraType = mc.options.getCameraType();
            mc.options.setCameraType(oldCameraType);
            mc.player.input = playerInput;
            if (cameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
                mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
            }
            oldCameraType = null;
        }
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
        while (KeyBindingsController.toggleCamControl.consumeClick()) {
            toggleCameraControl();
        }
        while (KeyBindingsController.toggleEyeLock.consumeClick()) {
            toggleEyeLock();
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
                        printSystemMessage(chatPrefix.copy()
                                .append(Component.literal("Current settings").withStyle(ChatFormatting.YELLOW)).append("\n")
                                .append(Component.literal("- maxspeed=" + config.maxSpeed).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(Component.literal("- acceleration=" + config.acceleration).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(Component.literal("- slowdown=" + config.slowdownFactor).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(Component.literal("- hands=" + (config.renderHands ? 1 : 0)).withStyle(ChatFormatting.WHITE)).append("\n")
                                .append(Component.literal("- interactions=" + (!config.disableInteractions ? 1 : 0)).withStyle(ChatFormatting.WHITE)));
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

                            case "interactions":
                            case "ia":
                                if (value == 0) {
                                    config.disableInteractions = true;
                                    saveConfig();
                                } else if (value == 1) {
                                    config.disableInteractions = false;
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

    public void onMouseTurn(double yRot, double xRot) {
        if (!eyeLock) {
            this.xRot += (float) xRot * 0.15F;
            this.yRot += (float) yRot * 0.15F;
            this.xRot = Mth.clamp(this.xRot, -90, 90);
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

            Input input = playerInput;
            float forwardImpulse = cameraControlActive ? (input.up ? 1 : 0) + (input.down ? -1 : 0) : 0;
            float leftImpulse = cameraControlActive ? (input.left ? 1 : 0) + (input.right ? -1 : 0) : 0;
            float upImpulse = cameraControlActive ? ((input.jumping ? 1 : 0) + (input.shiftKeyDown ? -1 : 0)) : 0;
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

            applyEyeLock();
        }
    }

    public void onClientTickStart() {
        if (active) {
            disableKey(mc.options.keyTogglePerspective);
            if (config.disableInteractions) {
                disableKey(mc.options.keyUse);
                disableKey(mc.options.keyAttack);
            }
            playerInput.tick(false, 0);
        }
    }

    public void onWorldUnload() {
        disable();
    }

    public boolean shouldOverridePlayerPosition() {
        return MixinGameRendererHelper.insidePick || insideRenderDebug;
    }

    public void onRenderDebugScreenLeft(List<String> list) {
        if (active) {
            list.add("");
            String coordinates = String.format(Locale.ROOT, "Free Cam XYZ: %.3f / %.5f / %.3f", x, y, z);
            list.add(coordinates);
        }
    }

    public void onRenderDebugScreenRight(List<String> list) {
        if (active) {
            insideRenderDebug = true;
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
                insideRenderDebug = false;
            }
        }
    }

    public boolean shouldRenderHands() {
        return config.renderHands && cameraControlActive && !eyeLock;
    }

    public boolean shouldRenderCrosshair() {
        return active && cameraControlActive && !eyeLock;
    }

    private void applyEyeLock() {
        if (eyeLock) {
            float frameTime = mc.getFrameTime();
            Entity entity = mc.getCameraEntity();
            double xe = Mth.lerp(frameTime, entity.xo, entity.getX());
            double ye = Mth.lerp(frameTime, entity.yo, entity.getY()) + entity.getEyeHeight();
            double ze = Mth.lerp(frameTime, entity.zo, entity.getZ());
            double dx = x - xe;
            double dy = y - ye;
            double dz = z - ze;
            this.xRot = (float) (Math.atan2(dy, Math.sqrt(dx * dx + dz * dz)) / Math.PI * 180);
            this.yRot = (float) (Math.atan2(dz, dx) / Math.PI * 180 + 90);
            this.xRot = Mth.clamp(this.xRot, -90, 90);
            calculateVectors();
        }
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
        printSystemMessage(chatPrefix.copy()
                .append(Component.literal(message).withStyle(ChatFormatting.GOLD)));
    }

    private void printError(String message) {
        printSystemMessage(chatPrefix.copy()
                .append(Component.literal(message).withStyle(ChatFormatting.RED)));
    }

    private void printHelp() {
        printSystemMessage(chatPrefix.copy()
                .append(Component.literal("Invalid syntax").withStyle(ChatFormatting.RED)).append("\n")
                .append(Component.literal("- ").withStyle(ChatFormatting.WHITE))
                        .append(Component.literal(".freecam maxspeed 50").withStyle(ChatFormatting.YELLOW))
                        .append(Component.literal(" set maximum speed, blocks/second").withStyle(ChatFormatting.WHITE))
                        .append("\n")
                        .append(Component.literal(" (synonyms: max, speed, s)").withStyle(ChatFormatting.AQUA))
                        .append("\n")
                .append(Component.literal("- ").withStyle(ChatFormatting.WHITE))
                    .append(Component.literal(".freecam acceleration 50").withStyle(ChatFormatting.YELLOW))
                    .append(Component.literal(" set acceleration speed, blocks/second^2").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                    .append(Component.literal(" (synonyms: acc, a)").withStyle(ChatFormatting.AQUA))
                    .append("\n")
                .append(Component.literal("- ").withStyle(ChatFormatting.WHITE))
                    .append(Component.literal(".freecam slowdown 0.01").withStyle(ChatFormatting.YELLOW))
                    .append(Component.literal(" set slow down speed. When no keys is pressed speed is multiplied by this value every second.").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                    .append(Component.literal(" (synonyms: slow, sd)").withStyle(ChatFormatting.AQUA))
                    .append("\n")
                .append(Component.literal("- ").withStyle(ChatFormatting.WHITE))
                    .append(Component.literal(".freecam hands 1").withStyle(ChatFormatting.YELLOW))
                    .append(Component.literal(" render hands while in freecam. Values: 0/1.").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                .append(Component.literal("- ").withStyle(ChatFormatting.WHITE))
                    .append(Component.literal(".freecam interactions 0").withStyle(ChatFormatting.YELLOW))
                    .append(Component.literal(" enable interactions (left/right mouse clicks) while in freecam. Values: 0/1.").withStyle(ChatFormatting.WHITE))
                    .append("\n")
                    .append(Component.literal(" (synonyms: ia)").withStyle(ChatFormatting.AQUA)));
    }

    private void printSystemMessage(Component component) {
        mc.getChatListener().handleSystemMessage(component, false);
    }
}