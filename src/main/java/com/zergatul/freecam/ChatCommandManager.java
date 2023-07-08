package com.zergatul.freecam;

import net.minecraft.client.Minecraft;
import net.minecraft.util.text.ChatType;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentString;
import net.minecraft.util.text.TextFormatting;

import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ChatCommandManager {

    public static final ChatCommandManager instance = new ChatCommandManager();

    private final ITextComponent chatPrefix = createText("[freecam]", TextFormatting.GREEN).appendText(" ");
    private final ParserEntry[] patterns = new ParserEntry[] {
            new ParserEntry(Pattern.compile("^\\.freecam$"), this::showCurrentSettings),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path$"), this::showPathInfo),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path\\s+clear$"), this::clearPath),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path\\s+add\\s+(?<value>\\S+)$"), setDoubleValue(this::addPathPoint)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(maxspeed|max|speed|s)\\s+(?<value>\\S+)$"), setDoubleValue(this::setMaxSpeed)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(acceleration|acc|a)\\s+(?<value>\\S+)$"), setDoubleValue(this::setAcceleration)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(slowdown|slow|sd)\\s+(?<value>\\S+)$"), setDoubleValue(this::setSlowDown)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(hands)\\s+(?<value>\\S+)$"), setDoubleValue(this::setHands)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(target)\\s+(?<value>\\S+)$"), setDoubleValue(this::setTarget)),
    };

    private ChatCommandManager() {

    }

    public boolean handleChatMessage(String message) {
        if (message == null) {
            return false;
        }

        message = message.toLowerCase(Locale.ROOT);
        if (!message.startsWith(".freecam")) {
            return false;
        }

        message = message.trim();

        FreeCamConfig config = FreeCam.instance.getConfig();
        boolean accepted = false;
        for (ParserEntry entry : patterns) {
            Matcher matcher = entry.pattern.matcher(message);
            if (matcher.matches()) {
                entry.consumer.accept(config, matcher);
                accepted = true;
                break;
            }
        }

        if (!accepted) {
            printHelp();
        }

        return true;
    }

    private void showCurrentSettings(FreeCamConfig config, Matcher matcher) {
        printSystemMessage(chatPrefix.createCopy()
                .appendSibling(createText("Current settings", TextFormatting.YELLOW)).appendText("\n")
                .appendSibling(createText("- maxspeed=" + config.maxSpeed, TextFormatting.WHITE)).appendText("\n")
                .appendSibling(createText("- acceleration=" + config.acceleration, TextFormatting.WHITE)).appendText("\n")
                .appendSibling(createText("- slowdown=" + config.slowdownFactor, TextFormatting.WHITE)).appendText("\n")
                .appendSibling(createText("- hands=" + (config.renderHands ? 1 : 0), TextFormatting.WHITE)).appendText("\n")
                .appendSibling(createText("- target=" + (config.target ? 1 : 0), TextFormatting.WHITE)));
    }

    private void showPathInfo(FreeCamConfig config, Matcher matcher) {
        printSystemMessage(chatPrefix.createCopy()
                .appendSibling(createText("Current path: ", TextFormatting.YELLOW))
                .appendSibling(createText(String.format("%s points", FreeCam.instance.getPath().get().size()), TextFormatting.WHITE)));
    }

    private void clearPath(FreeCamConfig config, Matcher matcher) {
        FreeCam.instance.getPath().get().clear();
    }

    private BiConsumer<FreeCamConfig, Matcher> setDoubleValue(BiConsumer<FreeCamConfig, Double> consumer) {
        return ((config, matcher) -> {
            double value;
            try {
                value = Double.parseDouble(matcher.group("value"));
            } catch (NumberFormatException e) {
                value = Double.NaN;
            }

            if (Double.isNaN(value)) {
                printError("Cannot parse value");
            } else {
                consumer.accept(config, value);
            }
        });
    }

    private void addPathPoint(FreeCamConfig config, double value) {
        FreeCamPath path = FreeCam.instance.getPath();
        boolean added = false;
        if (path.get().size() == 0) {
            added = FreeCam.instance.getPath().add(0);
        } else {
            if (value < 0 || value > 3600000) {
                printError("Delay out of range. Accepted values: 0..3600000");
            } else {
                added = FreeCam.instance.getPath().add(value);
            }
        }

        if (added) {
            printSystemMessage(chatPrefix.createCopy()
                    .appendSibling(createText("Point added", TextFormatting.YELLOW)));
        }
    }

    private void setMaxSpeed(FreeCamConfig config, double value) {
        if (value < FreeCamConfig.MinMaxSpeed || value > FreeCamConfig.MaxMaxSpeed) {
            printError("Value out of range. Allowed range: [" + FreeCamConfig.MinMaxSpeed + " - " + FreeCamConfig.MaxMaxSpeed + "]");
        } else {
            config.maxSpeed = value;
            saveConfig(config);
        }
    }

    private void setAcceleration(FreeCamConfig config, double value) {
        if (value < FreeCamConfig.MinAcceleration || value > FreeCamConfig.MaxAcceleration) {
            printError("Value out of range. Allowed range: [" + FreeCamConfig.MinAcceleration + " - " + FreeCamConfig.MaxAcceleration + "]");
        } else {
            config.acceleration = value;
            saveConfig(config);
        }
    }

    private void setSlowDown(FreeCamConfig config, double value) {
        if (value < FreeCamConfig.MinSlowdownFactor || value > FreeCamConfig.MaxSlowdownFactor) {
            printError("Value out of range. Allowed range: [" + FreeCamConfig.MinSlowdownFactor + " - " + FreeCamConfig.MaxSlowdownFactor + "]");
        } else {
            config.slowdownFactor = value;
            saveConfig(config);
        }
    }

    private void setHands(FreeCamConfig config, double value) {
        if (value == 0) {
            config.renderHands = false;
            saveConfig(config);
        } else if (value == 1) {
            config.renderHands = true;
            saveConfig(config);
        } else {
            printError("Invalid value. Only 0 or 1 accepted.");
        }
    }

    private void setTarget(FreeCamConfig config, double value) {
        if (value == 0) {
            config.target = false;
            saveConfig(config);
        } else if (value == 1) {
            config.target = true;
            saveConfig(config);
        } else {
            printError("Invalid value. Only 0 or 1 accepted.");
        }
    }

    private void saveConfig(FreeCamConfig config) {
        ConfigRepository.instance.save(config);
        printInfo("Config updated");
    }

    private void printInfo(String message) {
        printSystemMessage(chatPrefix.createCopy()
                .appendSibling(createText(message, TextFormatting.GOLD)));
    }

    private void printError(String message) {
        printSystemMessage(chatPrefix.createCopy()
                .appendSibling(createText(message, TextFormatting.RED)));
    }

    private void printHelp() {
        printSystemMessage(chatPrefix.createCopy()
                .appendSibling(createText("Invalid syntax", TextFormatting.RED))
                .appendText("\n")
                .appendSibling(createText("- ", TextFormatting.WHITE))
                .appendSibling(createText(".freecam maxspeed 50", TextFormatting.YELLOW))
                .appendSibling(createText(" set maximum speed, blocks/second", TextFormatting.WHITE))
                .appendText("\n")
                .appendSibling(createText(" (synonyms: max, speed, s)", TextFormatting.AQUA))
                .appendText("\n")
                .appendSibling(createText("- ", TextFormatting.WHITE))
                .appendSibling(createText(".freecam acceleration 50", TextFormatting.YELLOW))
                .appendSibling(createText(" set acceleration speed, blocks/second^2", TextFormatting.WHITE))
                .appendText("\n")
                .appendSibling(createText(" (synonyms: acc, a)", TextFormatting.AQUA))
                .appendText("\n")
                .appendSibling(createText("- ", TextFormatting.WHITE))
                .appendSibling(createText(".freecam slowdown 0.01", TextFormatting.YELLOW))
                .appendSibling(createText(" set slow down speed. When no keys is pressed speed is multiplied by this value every second.", TextFormatting.WHITE))
                .appendText("\n")
                .appendSibling(createText(" (synonyms: slow, sd)", TextFormatting.AQUA))
                .appendText("\n")
                .appendSibling(createText("- ", TextFormatting.WHITE))
                .appendSibling(createText(".freecam hands 1", TextFormatting.YELLOW))
                .appendSibling(createText(" render hands while in freecam. Values: 0/1.", TextFormatting.WHITE))
                .appendText("\n")
                .appendSibling(createText("- ", TextFormatting.WHITE))
                .appendSibling(createText(".freecam target 0", TextFormatting.YELLOW))
                .appendSibling(createText(" disable custom targeting in freecam. Values: 0/1.", TextFormatting.WHITE)));
    }

    private void printSystemMessage(ITextComponent component) {
        Minecraft.getMinecraft().ingameGUI.addChatMessage(ChatType.SYSTEM, component);
    }

    private ITextComponent createText(String str, TextFormatting color) {
        ITextComponent component = new TextComponentString(str);
        component.getStyle().setColor(color);
        return component;
    }

    private class ParserEntry {
        public final Pattern pattern;
        public final BiConsumer<FreeCamConfig, Matcher> consumer;

        public ParserEntry(Pattern pattern, BiConsumer<FreeCamConfig, Matcher> consumer) {
            this.pattern = pattern;
            this.consumer = consumer;
        }
    }
}