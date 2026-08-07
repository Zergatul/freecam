package com.zergatul.freecam.ui;

import com.zergatul.freecam.ConfigRepository;
import com.zergatul.freecam.FreeCamConfig;
import com.zergatul.freecam.FreeCam;
import cpw.mods.fml.client.config.GuiSlider;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.resources.I18n;

import java.text.DecimalFormat;
import java.util.List;
import java.util.function.Consumer;

@SuppressWarnings("unchecked")
public class FreeCamSettingsScreen extends GuiScreen implements GuiSlider.ISlider {

    private static final int ACCELERATION_ID = 0;
    private static final int MAX_SPEED_ID = 1;
    private static final int SLOWDOWN_ID = 2;
    private static final int RENDER_HANDS_ID = 3;
    private static final int TARGET_ID = 4;
    private static final int SPECTATOR_MOVEMENT_ID = 5;
    private static final int REMEMBER_INPUT_STATE_ID = 6;
    private static final int SHOW_MY_NAME_ID = 7;
    private static final int DONE_ID = 200;

    private static final int BUTTON_WIDTH = 150;
    private static final int BUTTON_HEIGHT = 20;
    private static final int DONE_BUTTON_WIDTH = 200;
    private static final int GAP = 12;
    private static final int TITLE_TOP = 20;
    private static final int BUTTONS_TOP = 40;
    private static final int LINE_WIDTH = 2 * BUTTON_WIDTH + GAP;
    private static final int LINE_HEIGHT = BUTTON_HEIGHT + GAP / 2;

    private final GuiScreen previous;
    private final ValueMapper accelerationMapper = new ExponentialValueMapper(
            FreeCamConfig.MinAcceleration,
            FreeCamConfig.DefaultAcceleration,
            FreeCamConfig.MaxAcceleration) {
        @Override
        public String toDisplay(double value) {
            return String.format("%.1f", toSettingValue(value));
        }
    };
    private final ValueMapper maxSpeedMapper = new ExponentialValueMapper(
            FreeCamConfig.MinMaxSpeed,
            FreeCamConfig.DefaultMaxSpeed,
            FreeCamConfig.MaxMaxSpeed) {
        @Override
        public String toDisplay(double value) {
            return String.format("%.1f", toSettingValue(value));
        }
    };
    private final ValueMapper slowdownMapper = new ExponentialValueMapper(
            FreeCamConfig.MaxSlowdownFactor,
            FreeCamConfig.DefaultSlowdownFactor,
            FreeCamConfig.MinSlowdownFactor) {
        @Override
        public String toDisplay(double value) {
            return Integer.toString((int) Math.round(value * 100));
        }
    };

    private boolean changed = false;

    public FreeCamSettingsScreen() {
        this(null);
    }

    public FreeCamSettingsScreen(GuiScreen previous) {
        this.previous = previous;
    }

    @Override
    public void initGui() {
        int column1 = (width - LINE_WIDTH) / 2;
        int column2 = column1 + BUTTON_WIDTH + GAP;
        int y = BUTTONS_TOP;
        FreeCamConfig config = FreeCam.INSTANCE.getConfig();

        addSlider(ACCELERATION_ID, column1, y, "options.freecam.settings.acceleration", accelerationMapper, config.acceleration);

        y += LINE_HEIGHT;
        addSlider(MAX_SPEED_ID, column1, y, "options.freecam.settings.maxspeed", maxSpeedMapper, config.maxSpeed);

        y += LINE_HEIGHT;
        addSlider(SLOWDOWN_ID, column1, y, "options.freecam.settings.slowdown", slowdownMapper, config.slowdownFactor);

        y += LINE_HEIGHT;
        buttonList.add(new GuiButton(
                RENDER_HANDS_ID,
                column1,
                y,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                getToggleText("options.freecam.settings.hands", config.renderHands)));
        buttonList.add(new GuiButton(
                TARGET_ID,
                column2,
                y,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                getToggleText("options.freecam.settings.target", config.target)));

        y += LINE_HEIGHT;
        buttonList.add(new GuiButton(
                SPECTATOR_MOVEMENT_ID,
                column1,
                y,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                getFlyModeText()));
        buttonList.add(new GuiButton(
                REMEMBER_INPUT_STATE_ID,
                column2,
                y,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                getToggleText("options.freecam.settings.remember.input", config.rememberInputState)));

        y += LINE_HEIGHT;
        buttonList.add(new GuiButton(
                SHOW_MY_NAME_ID,
                column1,
                y,
                BUTTON_WIDTH,
                BUTTON_HEIGHT,
                getToggleText("options.freecam.settings.show.name", config.showMyName)));

        y += 2 * LINE_HEIGHT;
        buttonList.add(new GuiButton(
                DONE_ID,
                (width - DONE_BUTTON_WIDTH) / 2,
                y,
                DONE_BUTTON_WIDTH,
                BUTTON_HEIGHT,
                I18n.format("gui.done")));
    }

    private void addSlider(int id, int x, int y, String translationKey, ValueMapper mapper, double value) {
        GuiSlider slider = new GuiSlider(
                id,
                x,
                y,
                LINE_WIDTH,
                BUTTON_HEIGHT,
                "",
                "",
                0,
                1,
                mapper.toSliderValue(value),
                true,
                true,
                this);
        updateSliderText(slider, translationKey, mapper);
        buttonList.add(slider);
    }

    @Override
    protected void actionPerformed(GuiButton button) {
        if (!button.enabled) {
            return;
        }

        if (button.id == RENDER_HANDS_ID) {
            update(config -> config.renderHands = !config.renderHands);
            button.displayString = getToggleText(
                    "options.freecam.settings.hands",
                    FreeCam.INSTANCE.getConfig().renderHands);
        } else if (button.id == TARGET_ID) {
            update(config -> config.target = !config.target);
            button.displayString = getToggleText(
                    "options.freecam.settings.target",
                    FreeCam.INSTANCE.getConfig().target);
        } else if (button.id == SPECTATOR_MOVEMENT_ID) {
            update(config -> config.spectatorMovement = !config.spectatorMovement);
            button.displayString = getFlyModeText();
        } else if (button.id == REMEMBER_INPUT_STATE_ID) {
            update(config -> config.rememberInputState = !config.rememberInputState);
            button.displayString = getToggleText(
                    "options.freecam.settings.remember.input",
                    FreeCam.INSTANCE.getConfig().rememberInputState);
        } else if (button.id == SHOW_MY_NAME_ID) {
            update(config -> config.showMyName = !config.showMyName);
            button.displayString = getToggleText(
                    "options.freecam.settings.show.name",
                    FreeCam.INSTANCE.getConfig().showMyName);
        } else if (button.id == DONE_ID) {
            closeScreen();
        }
    }

    @Override
    protected void keyTyped(char typedChar, int keyCode) {
        if (keyCode == 1) {
            closeScreen();
        } else {
            super.keyTyped(typedChar, keyCode);
        }
    }

    private void closeScreen() {
        mc.displayGuiScreen(previous);
    }

    @Override
    public void drawScreen(int mouseX, int mouseY, float partialTicks) {
        drawDefaultBackground();
        drawCenteredString(
                fontRendererObj,
                I18n.format("options.freecam.settings.title"),
                width / 2,
                TITLE_TOP,
                0xFFFFFF);
        super.drawScreen(mouseX, mouseY, partialTicks);

        String tooltip = getTooltip(mouseX, mouseY);
        if (tooltip != null) {
            List<String> lines = fontRendererObj.listFormattedStringToWidth(tooltip, Math.min(width - 20, 300));
            drawHoveringText(lines, mouseX, mouseY);
        }
    }

    private String getTooltip(int mouseX, int mouseY) {
        for (Object object : buttonList) {
            GuiButton button = (GuiButton) object;
            if (mouseX < button.xPosition || mouseY < button.yPosition ||
                    mouseX >= button.xPosition + button.width || mouseY >= button.yPosition + button.height) {
                continue;
            }

            switch (button.id) {
                case ACCELERATION_ID:
                    return I18n.format("options.freecam.settings.acceleration.tooltip");
                case MAX_SPEED_ID:
                    return I18n.format("options.freecam.settings.maxspeed.tooltip");
                case SLOWDOWN_ID:
                    return I18n.format(
                            "options.freecam.settings.slowdown.tooltip",
                            formatSlowdownFactor(FreeCam.INSTANCE.getConfig().slowdownFactor));
                case RENDER_HANDS_ID:
                    return I18n.format("options.freecam.settings.hands.tooltip");
                case TARGET_ID:
                    return I18n.format("options.freecam.settings.target.tooltip");
                case REMEMBER_INPUT_STATE_ID:
                    return I18n.format("options.freecam.settings.remember.input.tooltip");
                case SHOW_MY_NAME_ID:
                    return I18n.format("options.freecam.settings.show.name.tooltip");
                default:
                    return null;
            }
        }
        return null;
    }

    private String getToggleText(String translationKey, boolean value) {
        return I18n.format(translationKey) + ": " +
                I18n.format(value ? "options.on" : "options.off");
    }

    private String getFlyModeText() {
        FreeCamConfig config = FreeCam.INSTANCE.getConfig();
        return I18n.format("options.freecam.settings.flymode") + ": " + I18n.format(
                config.spectatorMovement ?
                        "options.freecam.settings.flymode.spectator" :
                        "options.freecam.settings.flymode.default");
    }

    private String formatSlowdownFactor(double factor) {
        if (factor < 0.01) {
            return new DecimalFormat("0.###E0").format(factor);
        }
        return new DecimalFormat("0.000").format(factor);
    }

    @Override
    public void onChangeSliderValue(GuiSlider slider) {
        double value = slider.getValue();
        switch (slider.id) {
            case ACCELERATION_ID:
                update(config -> config.acceleration = accelerationMapper.toSettingValue(value));
                updateSliderText(slider, "options.freecam.settings.acceleration", accelerationMapper);
                break;
            case MAX_SPEED_ID:
                update(config -> config.maxSpeed = maxSpeedMapper.toSettingValue(value));
                updateSliderText(slider, "options.freecam.settings.maxspeed", maxSpeedMapper);
                break;
            case SLOWDOWN_ID:
                update(config -> config.slowdownFactor = slowdownMapper.toSettingValue(value));
                updateSliderText(slider, "options.freecam.settings.slowdown", slowdownMapper);
                break;
        }
    }

    private void updateSliderText(GuiSlider slider, String translationKey, ValueMapper mapper) {
        slider.displayString = I18n.format(translationKey) + ": " + mapper.toDisplay(slider.getValue());
    }

    @Override
    public void onGuiClosed() {
        super.onGuiClosed();

        if (changed) {
            ConfigRepository.INSTANCE.save();
        }
    }

    private void update(Consumer<FreeCamConfig> consumer) {
        FreeCamConfig config = FreeCam.INSTANCE.getConfig();
        consumer.accept(config);
        changed = true;
    }
}