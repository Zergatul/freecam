package com.zergatul.freecam;

import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import net.minecraft.block.BlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.settings.PointOfView;
import net.minecraft.entity.Entity;
import net.minecraft.state.Property;
import net.minecraft.util.MovementInput;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.Util;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.vector.Quaternion;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.util.math.vector.Vector3f;
import net.minecraft.util.text.*;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.*;

public class FreeCamController {

    public static final FreeCamController instance = new FreeCamController();

    private final Minecraft mc = Minecraft.getInstance();
    private final Quaternion rotation = new Quaternion(0.0F, 0.0F, 0.0F, 1.0F);
    private final Vector3f forwards = new Vector3f(0.0F, 0.0F, 1.0F);
    private final Vector3f up = new Vector3f(0.0F, 1.0F, 0.0F);
    private final Vector3f left = new Vector3f(1.0F, 0.0F, 0.0F);
    private FreeCamConfig config = ConfigStore.instance.load();
    private boolean active;
    private PointOfView oldCameraType;
    private MovementInput oldInput;
    private double x, y, z;
    private float yRot, xRot;
    private double forwardVelocity;
    private double leftVelocity;
    private double upVelocity;
    private long lastTime;
    private boolean insideRenderDebug;
    private IFormattableTextComponent chatPrefix = new StringTextComponent("[freecam]").withStyle(TextFormatting.GREEN).append(" ");
    private ChatType chatType = ChatType.SYSTEM;

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

    public void enable() {
        if (!active) {
            active = true;
            oldCameraType = mc.options.getCameraType();
            oldInput = mc.player.input;
            mc.player.input = new MovementInput();
            mc.options.setCameraType(PointOfView.THIRD_PERSON_BACK);
            if (oldCameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
                mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
            }

            float frameTime = mc.getFrameTime();
            Entity entity = mc.getCameraEntity();
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
    }

    public void disable() {
        if (active) {
            active = false;
            PointOfView cameraType = mc.options.getCameraType();
            mc.options.setCameraType(oldCameraType);
            mc.player.input = oldInput;
            if (cameraType.isFirstPerson() != mc.options.getCameraType().isFirstPerson()) {
                mc.gameRenderer.checkEntityPostEffect(mc.options.getCameraType().isFirstPerson() ? mc.getCameraEntity() : null);
            }
            oldCameraType = null;
        }
    }

    public void onKeyInput() {
        if (mc.player == null) {
            return;
        }
        if (mc.screen != null) {
            return;
        }
        if (KeyBindingsController.toggleFreeCam.isDown()) {
            toggle();
        }
    }

    public void onClientChat(String message, ModApiWrapper.Event event) {
        if (message == null) {
            return;
        }
        message = message.toLowerCase(Locale.ROOT);
        if (message.startsWith(".freecam")) {
            String[] parts = message.split("\\s+");
            switch (parts.length) {
                case 1:
                    if (parts[0].equals(".freecam")) {
                        mc.gui.handleChat(chatType, chatPrefix.copy()
                                .append(new StringTextComponent("Current settings").withStyle(TextFormatting.YELLOW)).append("\n")
                                .append(new StringTextComponent("- maxspeed=" + config.maxSpeed).withStyle(TextFormatting.WHITE)).append("\n")
                                .append(new StringTextComponent("- acceleration=" + config.acceleration).withStyle(TextFormatting.WHITE)).append("\n")
                                .append(new StringTextComponent("- slowdown=" + config.slowdownFactor).withStyle(TextFormatting.WHITE)).append("\n")
                                .append(new StringTextComponent("- hands=" + (config.renderHands ? 1 : 0)).withStyle(TextFormatting.WHITE)),
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

    public void onMouseTurn(double yRot, double xRot) {
        this.xRot += (float)xRot * 0.15F;
        this.yRot += (float)yRot * 0.15F;
        this.xRot = MathHelper.clamp(this.xRot, -90, 90);
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

            MovementInput input = oldInput;
            float forwardImpulse = (input.up ? 1 : 0) + (input.down ? -1 : 0);
            float leftImpulse = (input.left ? 1 : 0) + (input.right ? -1 : 0);
            float upImpulse = ((input.jumping ? 1 : 0) + (input.shiftKeyDown ? -1 : 0));
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
            x += dx;
            y += dy;
            z += dz;
        }
    }

    public void onClientTickStart() {
        if (active) {
            while (mc.options.keyTogglePerspective.consumeClick()) {
                // consume clicks
            }
            oldInput.tick(false);
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
                RayTraceResult hit = mc.player.pick(20.0D, 0.0F, false);
                if (hit.getType() == RayTraceResult.Type.BLOCK) {
                    BlockPos pos = ((BlockRayTraceResult)hit).getBlockPos();
                    BlockState state = mc.level.getBlockState(pos);
                    list.add("");
                    list.add(TextFormatting.UNDERLINE + "Free Cam Targeted Block: " + pos.getX() + ", " + pos.getY() + ", " + pos.getZ());
                    list.add(String.valueOf(ForgeRegistries.BLOCKS.getKey(state.getBlock())));

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
    }

    public boolean shouldRenderHands() {
        return config.renderHands;
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

    private void saveConfig() {
        ConfigStore.instance.save(config);
        printInfo("Config updated");
    }

    private void printInfo(String message) {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new StringTextComponent(message).withStyle(TextFormatting.GOLD)),
                Util.NIL_UUID);
    }

    private void printError(String message) {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new StringTextComponent(message).withStyle(TextFormatting.RED)),
                Util.NIL_UUID);
    }

    private void printHelp() {
        mc.gui.handleChat(chatType, chatPrefix.copy()
                .append(new StringTextComponent("Invalid syntax").withStyle(TextFormatting.RED)).append("\n")
                .append(new StringTextComponent("- ").withStyle(TextFormatting.WHITE))
                        .append(new StringTextComponent(".freecam maxspeed 50").withStyle(TextFormatting.YELLOW))
                        .append(new StringTextComponent(" set maximum speed, blocks/second").withStyle(TextFormatting.WHITE))
                        .append("\n")
                        .append(new StringTextComponent(" (synonyms: max, speed, s)").withStyle(TextFormatting.AQUA))
                        .append("\n")
                .append(new StringTextComponent("- ").withStyle(TextFormatting.WHITE))
                    .append(new StringTextComponent(".freecam acceleration 50").withStyle(TextFormatting.YELLOW))
                    .append(new StringTextComponent(" set acceleration speed, blocks/second^2").withStyle(TextFormatting.WHITE))
                    .append("\n")
                    .append(new StringTextComponent(" (synonyms: acc, a)").withStyle(TextFormatting.AQUA))
                    .append("\n")
                .append(new StringTextComponent("- ").withStyle(TextFormatting.WHITE))
                    .append(new StringTextComponent(".freecam slowdown 0.01").withStyle(TextFormatting.YELLOW))
                    .append(new StringTextComponent(" set slow down speed. When no keys is pressed speed is multiplied by this value every second.").withStyle(TextFormatting.WHITE))
                    .append("\n")
                    .append(new StringTextComponent(" (synonyms: slow, sd)").withStyle(TextFormatting.AQUA))
                    .append("\n")
                .append(new StringTextComponent("- ").withStyle(TextFormatting.WHITE))
                    .append(new StringTextComponent(".freecam hands 1").withStyle(TextFormatting.YELLOW))
                    .append(new StringTextComponent(" render hands while in freecam. Values: 0/1.").withStyle(TextFormatting.WHITE)),
                Util.NIL_UUID);
    }
}