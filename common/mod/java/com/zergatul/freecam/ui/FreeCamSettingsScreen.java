package com.zergatul.freecam.ui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.zergatul.freecam.ConfigStore;
import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.FreeCamConfig;
import net.minecraft.client.gui.DialogTexts;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.widget.Widget;
import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.text.DecimalFormat;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class FreeCamSettingsScreen extends Screen {

    private static final ITextComponent TITLE = new TranslationTextComponent("options.freecam.settings.title");
    private static final ITextComponent ACCELERATION = new TranslationTextComponent("options.freecam.settings.acceleration");
    private static final ITextComponent ACCELERATION_TOOLTIP = new TranslationTextComponent("options.freecam.settings.acceleration.tooltip");
    private static final ITextComponent MAX_SPEED = new TranslationTextComponent("options.freecam.settings.maxspeed");
    private static final ITextComponent MAX_SPEED_TOOLTIP = new TranslationTextComponent("options.freecam.settings.maxspeed.tooltip");
    private static final ITextComponent SLOWDOWN = new TranslationTextComponent("options.freecam.settings.slowdown");
    private static final ITextComponent TARGET = new TranslationTextComponent("options.freecam.settings.target");
    private static final ITextComponent TARGET_TOOLTIP = new TranslationTextComponent("options.freecam.settings.target.tooltip");
    private static final ITextComponent HANDS = new TranslationTextComponent("options.freecam.settings.hands");
    private static final ITextComponent HANDS_TOOLTIP = new TranslationTextComponent("options.freecam.settings.hands.tooltip");
    private static final ITextComponent INPUT = new TranslationTextComponent("options.freecam.settings.remember.input");
    private static final ITextComponent INPUT_TOOLTIP = new TranslationTextComponent("options.freecam.settings.remember.input.tooltip");
    private static final ITextComponent FLY_MODE = new TranslationTextComponent("options.freecam.settings.flymode");
    private static final ITextComponent FLY_MODE_DEFAULT = new TranslationTextComponent("options.freecam.settings.flymode.default");
    private static final ITextComponent FLY_MODE_SPECTATOR = new TranslationTextComponent("options.freecam.settings.flymode.spectator");
    private static final ITextComponent SHOW_MY_NAME = new TranslationTextComponent("options.freecam.settings.show.name");
    private static final ITextComponent SHOW_MY_NAME_TOOLTIP = new TranslationTextComponent("options.freecam.settings.show.name.tooltip");
    private static final int BUTTON_WIDTH = 150;
    private static final int BUTTON_HEIGHT = 20;
    private static final int DONE_BUTTON_WIDTH = 200;
    private static final int GAP = 12;
    private static final int TITLE_TOP = 20;
    private static final int BUTTONS_TOP = 40;
    private static final int LINE_WIDTH = 2 * BUTTON_WIDTH + GAP;
    private static final int LINE_HEIGHT = BUTTON_HEIGHT + GAP / 2;

    private final Screen previous;
    private final Map<Widget, Supplier<ITextComponent>> tooltips = new LinkedHashMap<>();
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
        tooltips.clear();

        int column1 = (this.width - GAP) / 2 - BUTTON_WIDTH;
        int column2 = (this.width + GAP) / 2;
        FreeCamConfig config = FreeCam.INSTANCE.getConfig();

        int y = BUTTONS_TOP;
        addTooltip(addButton(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(ACCELERATION)
                .mapper(new ExponentialValueMapper(FreeCamConfig.MIN_ACCELERATION, FreeCamConfig.DEFAULT_ACCELERATION, FreeCamConfig.MAX_ACCELERATION) {
                    @Override
                    public String toDisplay(double value) {
                        return String.format("%.1f", toSettingValue(value));
                    }
                })
                .setter((button, value) -> update(current -> current.acceleration = value))
                .value(config.acceleration)
                .create()), () -> ACCELERATION_TOOLTIP);

        y += LINE_HEIGHT;
        addTooltip(addButton(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(MAX_SPEED)
                .mapper(new ExponentialValueMapper(FreeCamConfig.MIN_MAX_SPEED, FreeCamConfig.DEFAULT_MAX_SPEED, FreeCamConfig.MAX_MAX_SPEED) {
                    @Override
                    public String toDisplay(double value) {
                        return String.format("%.1f", toSettingValue(value));
                    }
                })
                .setter((button, value) -> update(current -> current.maxSpeed = value))
                .value(config.maxSpeed)
                .create()), () -> MAX_SPEED_TOOLTIP);

        y += LINE_HEIGHT;
        addTooltip(addButton(new SliderButton.Builder()
                .position(column1, y)
                .size(LINE_WIDTH, BUTTON_HEIGHT)
                .message(SLOWDOWN)
                .mapper(new ExponentialValueMapper(FreeCamConfig.MAX_SLOWDOWN_FACTOR, FreeCamConfig.DEFAULT_SLOWDOWN_FACTOR, FreeCamConfig.MIN_SLOWDOWN_FACTOR) {
                    @Override
                    public String toDisplay(double value) {
                        return Integer.toString((int) Math.round(value * 100));
                    }
                })
                .setter((button, value) -> update(current -> current.slowdownFactor = value))
                .value(config.slowdownFactor)
                .create()), () -> new TranslationTextComponent(
                        "options.freecam.settings.slowdown.tooltip",
                        formatSlowdownFactor(FreeCam.INSTANCE.getConfig().slowdownFactor)));

        y += LINE_HEIGHT;
        Button handsButton = addButton(new Button(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, getToggleText(HANDS, config.renderHands), button -> {
            update(current -> current.renderHands = !current.renderHands);
            button.setMessage(getToggleText(HANDS, FreeCam.INSTANCE.getConfig().renderHands));
        }));
        addTooltip(handsButton, () -> HANDS_TOOLTIP);

        Button targetButton = addButton(new Button(column2, y, BUTTON_WIDTH, BUTTON_HEIGHT, getToggleText(TARGET, config.target), button -> {
            update(current -> current.target = !current.target);
            button.setMessage(getToggleText(TARGET, FreeCam.INSTANCE.getConfig().target));
        }));
        addTooltip(targetButton, () -> TARGET_TOOLTIP);

        y += LINE_HEIGHT;
        addButton(new Button(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, getFlyModeText(config), button -> {
            update(current -> current.spectatorMovement = !current.spectatorMovement);
            button.setMessage(getFlyModeText(FreeCam.INSTANCE.getConfig()));
        }));

        Button inputButton = addButton(new Button(column2, y, BUTTON_WIDTH, BUTTON_HEIGHT, getToggleText(INPUT, config.rememberInputState), button -> {
            update(current -> current.rememberInputState = !current.rememberInputState);
            button.setMessage(getToggleText(INPUT, FreeCam.INSTANCE.getConfig().rememberInputState));
        }));
        addTooltip(inputButton, () -> INPUT_TOOLTIP);

        y += LINE_HEIGHT;
        Button nameButton = addButton(new Button(column1, y, BUTTON_WIDTH, BUTTON_HEIGHT, getToggleText(SHOW_MY_NAME, config.showMyName), button -> {
            update(current -> current.showMyName = !current.showMyName);
            button.setMessage(getToggleText(SHOW_MY_NAME, FreeCam.INSTANCE.getConfig().showMyName));
        }));
        addTooltip(nameButton, () -> SHOW_MY_NAME_TOOLTIP);

        y += 2 * LINE_HEIGHT;
        addButton(new Button((width - DONE_BUTTON_WIDTH) / 2, y, DONE_BUTTON_WIDTH, BUTTON_HEIGHT, DialogTexts.GUI_DONE, button -> onClose()));
    }

    @Override
    public void onClose() {
        this.minecraft.setScreen(previous);
    }

    @Override
    public void removed() {
        super.removed();
        if (changed) {
            ConfigStore.INSTANCE.save(FreeCam.INSTANCE.getConfig());
        }
    }

    @Override
    public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
        renderBackground(matrixStack);
        drawCenteredString(matrixStack, this.font, TITLE, this.width / 2, TITLE_TOP, 0xFFFFFF);
        super.render(matrixStack, mouseX, mouseY, partialTicks);

        for (Map.Entry<Widget, Supplier<ITextComponent>> entry : tooltips.entrySet()) {
            if (entry.getKey().isMouseOver(mouseX, mouseY)) {
                renderTooltip(matrixStack, this.font.split(entry.getValue().get(), Math.min(width - 20, 300)), mouseX, mouseY);
                break;
            }
        }
    }

    private void addTooltip(Widget widget, Supplier<ITextComponent> tooltip) {
        tooltips.put(widget, tooltip);
    }

    private ITextComponent getToggleText(ITextComponent label, boolean value) {
        return DialogTexts.optionStatus(label, value);
    }

    private ITextComponent getFlyModeText(FreeCamConfig config) {
        IFormattableTextComponent text = FLY_MODE.copy().append(": ");
        return text.append(config.spectatorMovement ? FLY_MODE_SPECTATOR : FLY_MODE_DEFAULT);
    }

    private String formatSlowdownFactor(double factor) {
        if (factor < 0.01) {
            return new DecimalFormat("0.###E0").format(factor);
        }
        return new DecimalFormat("0.000").format(factor);
    }

    private void update(Consumer<FreeCamConfig> consumer) {
        consumer.accept(FreeCam.INSTANCE.getConfig());
        changed = true;
    }
}