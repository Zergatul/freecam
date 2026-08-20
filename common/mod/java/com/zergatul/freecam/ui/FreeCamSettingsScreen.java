package com.zergatul.freecam.ui;

import com.mojang.blaze3d.vertex.PoseStack;
import com.zergatul.freecam.ConfigRepository;
import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.FreeCamConfig;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.CycleButton;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class FreeCamSettingsScreen extends Screen {

    private static final Component TITLE = new TranslatableComponent("options.freecam.settings.title");
    private static final Component ACCELERATION = new TranslatableComponent("options.freecam.settings.acceleration");
    private static final Component ACCELERATION_TOOLTIP = new TranslatableComponent("options.freecam.settings.acceleration.tooltip");
    private static final Component MAX_SPEED = new TranslatableComponent("options.freecam.settings.maxspeed");
    private static final Component MAX_SPEED_TOOLTIP = new TranslatableComponent("options.freecam.settings.maxspeed.tooltip");
    private static final Component SLOWDOWN = new TranslatableComponent("options.freecam.settings.slowdown");
    private static final Component TARGET = new TranslatableComponent("options.freecam.settings.target");
    private static final Component TARGET_TOOLTIP = new TranslatableComponent("options.freecam.settings.target.tooltip");
    private static final Component HANDS = new TranslatableComponent("options.freecam.settings.hands");
    private static final Component HANDS_TOOLTIP = new TranslatableComponent("options.freecam.settings.hands.tooltip");
    private static final Component INPUT = new TranslatableComponent("options.freecam.settings.remember.input");
    private static final Component INPUT_TOOLTIP = new TranslatableComponent("options.freecam.settings.remember.input.tooltip");
    private static final Component FLY_MODE = new TranslatableComponent("options.freecam.settings.flymode");
    private static final Component FLY_MODE_DEFAULT = new TranslatableComponent("options.freecam.settings.flymode.default");
    private static final Component FLY_MODE_SPECTATOR = new TranslatableComponent("options.freecam.settings.flymode.spectator");
    private static final Component SHOW_MY_NAME = new TranslatableComponent("options.freecam.settings.show.name");
    private static final Component SHOW_MY_NAME_TOOLTIP = new TranslatableComponent("options.freecam.settings.show.name.tooltip");
    private static final int BUTTON_WIDTH = 150;
    private static final int BUTTON_HEIGHT = 20;
    private static final int DONE_BUTTON_WIDTH = 200;
    private static final int GAP = 12;
    private static final int TITLE_TOP = 20;
    private static final int BUTTONS_TOP = 40;
    private static final int LINE_WIDTH = 2 * BUTTON_WIDTH + GAP;
    private static final int LINE_HEIGHT = BUTTON_HEIGHT + GAP / 2;

    private final Screen previous;
    private boolean changed;

    public FreeCamSettingsScreen() {
        this(null);
    }

    public FreeCamSettingsScreen(Screen previous) {
        super(TITLE);
        this.previous = previous;
    }

    @Override
    protected void init() {
        super.init();

        int column1 = (this.width - GAP) / 2 - BUTTON_WIDTH;
        int column2 = (this.width + GAP) / 2;

        int y = BUTTONS_TOP;
        addRenderableWidget(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(ACCELERATION)
                //.tooltip(Tooltip.create(ACCELERATION_TOOLTIP))
                .mapper(new ExponentialValueMapper(FreeCamConfig.MinAcceleration, FreeCamConfig.DefaultAcceleration, FreeCamConfig.MaxAcceleration) {
                    @Override
                    public String toDisplay(double value) {
                        return String.format("%.1f", toSettingValue(value));
                    }
                })
                .setter((button, value) -> update(config -> config.acceleration = value))
                .value(FreeCam.INSTANCE.getConfig().acceleration)
                .create());

        y += LINE_HEIGHT;
        addRenderableWidget(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(MAX_SPEED)
                //.tooltip(Tooltip.create(MAX_SPEED_TOOLTIP))
                .mapper(new ExponentialValueMapper(FreeCamConfig.MinMaxSpeed, FreeCamConfig.DefaultMaxSpeed, FreeCamConfig.MaxMaxSpeed) {
                    @Override
                    public String toDisplay(double value) {
                        return String.format("%.1f", toSettingValue(value));
                    }
                })
                .setter((button, value) -> update(config -> config.maxSpeed = value))
                .value(FreeCam.INSTANCE.getConfig().maxSpeed)
                .create());

        y += LINE_HEIGHT;
        addRenderableWidget(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(SLOWDOWN)
                /*.tooltipProvider((value, mapper) -> {
                    double factor = mapper.toSettingValue(value);
                    String str;
                    if (factor < 0.01) {
                        str = new DecimalFormat("0.###E0").format(factor);
                    } else {
                        str = new DecimalFormat("0.000").format(factor);
                    }
                    return Tooltip.create(new TranslatableComponent("options.freecam.settings.slowdown.tooltip", str));
                })*/
                .mapper(new ExponentialValueMapper(FreeCamConfig.MaxSlowdownFactor, FreeCamConfig.DefaultSlowdownFactor, FreeCamConfig.MinSlowdownFactor) {
                    @Override
                    public String toDisplay(double value) {
                        return Integer.toString((int) Math.round(value * 100));
                    }
                })
                .setter((button, value) -> update(config -> config.slowdownFactor = value))
                .value(FreeCam.INSTANCE.getConfig().slowdownFactor)
                .create());

        y += LINE_HEIGHT;
        addRenderableWidget(CycleButton.onOffBuilder()
                .withInitialValue(FreeCam.INSTANCE.getConfig().renderHands)
                .withTooltip(value -> List.of(HANDS_TOOLTIP.getVisualOrderText()))
                .create(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, HANDS, createListener((config, value) -> config.renderHands = value)));
        addRenderableWidget(CycleButton.onOffBuilder()
                .withInitialValue(FreeCam.INSTANCE.getConfig().target)
                .withTooltip(value -> List.of(TARGET_TOOLTIP.getVisualOrderText()))
                .create(column2, y, BUTTON_WIDTH, BUTTON_HEIGHT, TARGET, createListener((config, value) -> config.target = value)));

        y += LINE_HEIGHT;
        addRenderableWidget(new CycleButton.Builder<Boolean>(b -> b ? FLY_MODE_SPECTATOR : FLY_MODE_DEFAULT)
                .withValues(false, true)
                .withInitialValue(FreeCam.INSTANCE.getConfig().spectatorMovement)
                .create(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, FLY_MODE, createListener((config, value) -> config.spectatorMovement = value)));
        addRenderableWidget(CycleButton.onOffBuilder()
                .withInitialValue(FreeCam.INSTANCE.getConfig().rememberInputState)
                .withTooltip(value -> List.of(INPUT_TOOLTIP.getVisualOrderText()))
                .create(column2, y, BUTTON_WIDTH, BUTTON_HEIGHT, INPUT, createListener((config, value) -> config.rememberInputState = value)));

        y += LINE_HEIGHT;
        addRenderableWidget(CycleButton.onOffBuilder()
                .withInitialValue(FreeCam.INSTANCE.getConfig().showMyName)
                .withTooltip(value -> List.of(SHOW_MY_NAME_TOOLTIP.getVisualOrderText()))
                .create(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, SHOW_MY_NAME, createListener((config, value) -> config.showMyName = value)));

        y += 2 * LINE_HEIGHT;
        addRenderableWidget(new Button((width - DONE_BUTTON_WIDTH) / 2, y, DONE_BUTTON_WIDTH, BUTTON_HEIGHT, CommonComponents.GUI_DONE, button -> this.onClose()));
    }

    @Override
    public void onClose() {
        if (changed) {
            ConfigRepository.INSTANCE.save(FreeCam.INSTANCE.getConfig());
        }

        if (previous != null) {
            Minecraft.getInstance().setScreen(previous);
        } else {
            super.onClose();
        }
    }

    @Override
    public void render(PoseStack poseStack, int p_96563_, int p_96564_, float p_96565_) {
        renderBackground(poseStack);
        drawCenteredString(poseStack, this.font, TITLE, this.width / 2, TITLE_TOP, 16777215);
        super.render(poseStack, p_96563_, p_96564_, p_96565_);
    }

    private CycleButton.OnValueChange<Boolean> createListener(BiConsumer<FreeCamConfig, Boolean> consumer) {
        return (button, value) -> update(config -> consumer.accept(config, value));
    }

    private void update(Consumer<FreeCamConfig> consumer) {
        consumer.accept(FreeCam.INSTANCE.getConfig());
        changed = true;
    }
}