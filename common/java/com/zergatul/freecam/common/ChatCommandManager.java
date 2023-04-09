package com.zergatul.freecam.common;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;

import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ChatCommandManager {

    public static final ChatCommandManager instance = new ChatCommandManager();

    private final MutableComponent chatPrefix = Component.literal("[freecam]").withStyle(ChatFormatting.GREEN).append(" ");
    private final ParserEntry[] patterns = new ParserEntry[] {
            new ParserEntry(Pattern.compile("^\\.freecam$"), this::showCurrentSettings),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path$"), this::showPathInfo),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path\\s+clear$"), this::clearPath),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+path\\s+add\\s+(?<value>\\S+)$"), setDoubleValue(this::addPathPoint)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(maxspeed|max|speed|s)\\s+(?<value>\\S+)$"), setDoubleValue(this::setMaxSpeed)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(acceleration|acc|a)\\s+(?<value>\\S+)$"), setDoubleValue(this::setAcceleration)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(slowdown|slow|sd)\\s+(?<value>\\S+)$"), setDoubleValue(this::setSlowDown)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(hands)\s+(?<value>\\S+)$"), setDoubleValue(this::setHands)),
            new ParserEntry(Pattern.compile("^\\.freecam\\s+(target)\s+(?<value>\\S+)$"), setDoubleValue(this::setTarget)),
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
        printSystemMessage(chatPrefix.copy()
                .append(Component.literal("Current settings").withStyle(ChatFormatting.YELLOW)).append("\n")
                .append(Component.literal("- maxspeed=" + config.maxSpeed).withStyle(ChatFormatting.WHITE)).append("\n")
                .append(Component.literal("- acceleration=" + config.acceleration).withStyle(ChatFormatting.WHITE)).append("\n")
                .append(Component.literal("- slowdown=" + config.slowdownFactor).withStyle(ChatFormatting.WHITE)).append("\n")
                .append(Component.literal("- hands=" + (config.renderHands ? 1 : 0)).withStyle(ChatFormatting.WHITE)).append("\n")
                .append(Component.literal("- target=" + (config.target ? 1 : 0)).withStyle(ChatFormatting.WHITE)));
    }

    private void showPathInfo(FreeCamConfig config, Matcher matcher) {
        printSystemMessage(chatPrefix.copy()
                .append(Component.literal("Current path: ").withStyle(ChatFormatting.YELLOW))
                .append(Component.literal(String.format("%s points", FreeCam.instance.getPath().get().size())).withStyle(ChatFormatting.WHITE)));
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
            printSystemMessage(chatPrefix.copy()
                    .append(Component.literal("Point added").withStyle(ChatFormatting.YELLOW)));
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
                .append(Component.literal(".freecam target 0").withStyle(ChatFormatting.YELLOW))
                .append(Component.literal(" disable custom targeting in freecam. Values: 0/1.").withStyle(ChatFormatting.WHITE)));
    }

    private void printSystemMessage(Component component) {
        Minecraft.getInstance().getChatListener().handleSystemMessage(component, false);
    }

    private record ParserEntry(Pattern pattern, BiConsumer<FreeCamConfig, Matcher> consumer) {}
}