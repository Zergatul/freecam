package com.zergatul.freecam;

import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import net.minecraft.block.BlockState;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.input.Input;
import net.minecraft.client.option.Perspective;
import net.minecraft.entity.Entity;
import net.minecraft.network.MessageType;
import net.minecraft.state.property.Property;
import net.minecraft.text.LiteralText;
import net.minecraft.text.MutableText;
import net.minecraft.util.Formatting;
import net.minecraft.util.Util;
import net.minecraft.util.hit.BlockHitResult;
import net.minecraft.util.hit.HitResult;
import net.minecraft.util.math.*;
import net.minecraft.util.registry.Registry;

import java.util.List;
import java.util.Locale;
import java.util.Map;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private final MinecraftClient mc = MinecraftClient.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vec3f forwards = new Vec3f(0.0F, 0.0F, 1.0F);
    private final Vec3f up = new Vec3f(0.0F, 1.0F, 0.0F);
    private final Vec3f left = new Vec3f(1.0F, 0.0F, 0.0F);
    private FreeCamConfig config = ConfigStore.instance.load();
    private boolean active;
    private Perspective oldPerspective;
    private Input oldInput;
    private double x, y, z;
    private float yaw, pitch;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private boolean insideRenderDebugHud;
    private MutableText chatPrefix = new LiteralText("[freecam]").formatted(Formatting.GREEN).append(" ");
    private MessageType chatType = MessageType.SYSTEM;

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
        return pitch;
    }

    public float getYRot() {
        return yaw;
    }

    private void toggle() {
        if (active) {
            disable();
        } else {
            enable();
        }
    }

    private void enable() {
        if (!active) {
            active = true;
            oldPerspective = mc.options.getPerspective();
            oldInput = mc.player.input;
            mc.player.input = new Input();
            mc.options.setPerspective(Perspective.THIRD_PERSON_BACK);
            if (oldPerspective.isFirstPerson() != mc.options.getPerspective().isFirstPerson()) {
                mc.gameRenderer.onCameraEntitySet(mc.options.getPerspective().isFirstPerson() ? mc.getCameraEntity() : null);
            }

            float frameTime = mc.getLastFrameDuration();
            Entity entity = mc.getCameraEntity();
            x = MathHelper.lerp(frameTime, entity.lastRenderX, entity.getX());
            y = MathHelper.lerp(frameTime, entity.lastRenderY, entity.getY()) + entity.getStandingEyeHeight();
            z = MathHelper.lerp(frameTime, entity.lastRenderZ, entity.getZ());
            yaw = entity.getYaw(frameTime);
            pitch = entity.getPitch(frameTime);

            calculateVectors();

            double distance = -2;
            x += (double)this.forwards.getX() * distance;
            y += (double)this.forwards.getY() * distance;
            z += (double)this.forwards.getZ() * distance;

            forwardVelocity = 0;
            leftVelocity = 0;
            upVelocity = 0;
            lastTime = 0;
        }
    }

    private void disable() {
        if (active) {
            active = false;
            Perspective cameraType = mc.options.getPerspective();
            mc.options.setPerspective(oldPerspective);
            mc.player.input = oldInput;
            if (cameraType.isFirstPerson() != mc.options.getPerspective().isFirstPerson()) {
                mc.gameRenderer.onCameraEntitySet(mc.options.getPerspective().isFirstPerson() ? mc.getCameraEntity() : null);
            }
            oldPerspective = null;
        }
    }

    public void onKeyInput() {
        if (mc.player == null) {
            return;
        }
        if (mc.currentScreen != null) {
            return;
        }
        if (KeyBindingsController.instance.getKeyBinding().isPressed()) {
            toggle();
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
                        mc.inGameHud.addChatMessage(chatType, chatPrefix.shallowCopy()
                                .append(new LiteralText("Current settings").formatted(Formatting.YELLOW)).append("\n")
                                .append(new LiteralText("- maxspeed=" + config.maxSpeed).formatted(Formatting.WHITE)).append("\n")
                                .append(new LiteralText("- acceleration=" + config.acceleration).formatted(Formatting.WHITE)).append("\n")
                                .append(new LiteralText("- slowdown=" + config.slowdownFactor).formatted(Formatting.WHITE)).append("\n")
                                .append(new LiteralText("- hands=" + (config.renderHands ? 1 : 0)).formatted(Formatting.WHITE)),
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

    public void onMouseTurn(double cursorDeltaX, double cursorDeltaY) {
        this.pitch += (float)cursorDeltaY * 0.15F;
        this.yaw += (float)cursorDeltaX * 0.15F;
        this.pitch = MathHelper.clamp(this.pitch, -90, 90);
        calculateVectors();
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

            Input input = oldInput;
            float forwardImpulse = (input.pressingForward ? 1 : 0) + (input.pressingBack ? -1 : 0);
            float leftImpulse = (input.pressingLeft ? 1 : 0) + (input.pressingRight ? -1 : 0);
            float upImpulse = ((input.jumping ? 1 : 0) + (input.sneaking ? -1 : 0));
            double slowdown = Math.pow(config.slowdownFactor, frameTime);
            forwardVelocity = combineMovement(forwardVelocity, forwardImpulse, frameTime, config.acceleration, slowdown);
            leftVelocity = combineMovement(leftVelocity, leftImpulse, frameTime, config.acceleration, slowdown);
            upVelocity = combineMovement(upVelocity, upImpulse, frameTime, config.acceleration, slowdown);

            double dx = (double) this.forwards.getX() * forwardVelocity + (double) this.left.getX() * leftVelocity;
            double dy = (double) this.forwards.getY() * forwardVelocity + upVelocity + (double) this.left.getY() * leftVelocity;
            double dz = (double) this.forwards.getZ() * forwardVelocity + (double) this.left.getZ() * leftVelocity;
            dx *= frameTime;
            dy *= frameTime;
            dz *= frameTime;
            double speed = new Vec3d(dx, dy, dz).length() / frameTime;
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

    public void onClientTickStart() {
        if (active) {
            while (mc.options.togglePerspectiveKey.wasPressed()) {
                // consume clicks
            }
            oldInput.tick(false);
        }
    }

    public void onWorldUnload() {
        disable();
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
            insideRenderDebugHud = true;
            try {
                HitResult hit = mc.player.raycast(20.0D, 0.0F, false);
                if (hit.getType() == HitResult.Type.BLOCK) {
                    BlockPos pos = ((BlockHitResult)hit).getBlockPos();
                    BlockState state = mc.world.getBlockState(pos);
                    list.add("");
                    list.add(Formatting.UNDERLINE + "Free Cam Targeted Block: " + pos.getX() + ", " + pos.getY() + ", " + pos.getZ());
                    list.add(String.valueOf(Registry.BLOCK.getId(state.getBlock())));

                    for (var entry: state.getEntries().entrySet()) {
                        list.add(propertyToString(entry));
                    }

                    state.streamTags().map(tag -> "#" + tag.id()).forEach(list::add);
                }
            }
            finally {
                insideRenderDebugHud = false;
            }
        }
    }

    public boolean shouldOverridePlayerPosition() {
        return MixinGameRendererHelper.insideUpdateTargetedEntity || insideRenderDebugHud;
    }

    public boolean shouldRenderHands() {
        return config.renderHands;
    }

    private void calculateVectors() {
        rotation.set(0.0F, 0.0F, 0.0F, 1.0F);
        rotation.hamiltonProduct(Vec3f.POSITIVE_Y.getDegreesQuaternion(-yaw));
        rotation.hamiltonProduct(Vec3f.POSITIVE_X.getDegreesQuaternion(pitch));
        forwards.set(0.0F, 0.0F, 1.0F);
        forwards.rotate(rotation);
        up.set(0.0F, 1.0F, 0.0F);
        up.rotate(rotation);
        left.set(1.0F, 0.0F, 0.0F);
        left.rotate(rotation);
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

    private String propertyToString(Map.Entry<Property<?>, Comparable<?>> propEntry) {
        Property<?> property = propEntry.getKey();
        Comparable<?> comparable = propEntry.getValue();
        String string = Util.getValueAsString(property, comparable);
        if (Boolean.TRUE.equals(comparable)) {
            string = Formatting.GREEN + string;
        } else if (Boolean.FALSE.equals(comparable)) {
            string = Formatting.RED + string;
        }

        return property.getName() + ": " + string;
    }

    private void saveConfig() {
        ConfigStore.instance.save(config);
        printInfo("Config updated");
    }

    private void printInfo(String message) {
        mc.inGameHud.addChatMessage(chatType, chatPrefix.shallowCopy()
                .append(new LiteralText(message).formatted(Formatting.GOLD)),
                Util.NIL_UUID);
    }

    private void printError(String message) {
        mc.inGameHud.addChatMessage(chatType, chatPrefix.shallowCopy()
                .append(new LiteralText(message).formatted(Formatting.RED)),
                Util.NIL_UUID);
    }

    private void printHelp() {
        mc.inGameHud.addChatMessage(chatType, chatPrefix.shallowCopy()
                .append(new LiteralText("Invalid syntax").formatted(Formatting.RED)).append("\n")
                .append(new LiteralText("- ").formatted(Formatting.WHITE))
                .append(new LiteralText(".freecam maxspeed 50").formatted(Formatting.YELLOW))
                .append(new LiteralText(" set maximum speed, blocks/second").formatted(Formatting.WHITE))
                .append("\n")
                .append(new LiteralText(" (synonyms: max, speed, s)").formatted(Formatting.AQUA))
                .append("\n")
                .append(new LiteralText("- ").formatted(Formatting.WHITE))
                .append(new LiteralText(".freecam acceleration 50").formatted(Formatting.YELLOW))
                .append(new LiteralText(" set acceleration speed, blocks/second^2").formatted(Formatting.WHITE))
                .append("\n")
                .append(new LiteralText(" (synonyms: acc, a)").formatted(Formatting.AQUA))
                .append("\n")
                .append(new LiteralText("- ").formatted(Formatting.WHITE))
                .append(new LiteralText(".freecam slowdown 0.01").formatted(Formatting.YELLOW))
                .append(new LiteralText(" set slow down speed. When no keys is pressed speed is multiplied by this value every second.").formatted(Formatting.WHITE))
                .append("\n")
                .append(new LiteralText(" (synonyms: slow, sd)").formatted(Formatting.AQUA))
                .append("\n")
                .append(new LiteralText("- ").formatted(Formatting.WHITE))
                .append(new LiteralText(".freecam hands 1").formatted(Formatting.YELLOW))
                .append(new LiteralText(" render hands while in freecam. Values: 0/1.").formatted(Formatting.WHITE)),
                Util.NIL_UUID);
    }
}