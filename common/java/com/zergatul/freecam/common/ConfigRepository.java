package com.zergatul.freecam.common;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import net.minecraft.client.Minecraft;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;

public class ConfigRepository {

    public static final ConfigRepository instance = new ConfigRepository();

    private static final String FILE = "zergatul.freecam.json";

    private final Logger logger = LogManager.getLogger(ConfigRepository.class);
    private final Gson gson = new GsonBuilder()
            .setPrettyPrinting()
            .create();

    private ConfigRepository() {

    }

    public void save(FreeCamConfig config) {
        File file = getFile();
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(file));
            gson.toJson(config, writer);
            writer.close();
        }
        catch (Exception e) {
            logger.warn("Cannot write config", e);
        }
    }

    public FreeCamConfig load() {
        File file = getFile();
        if (file.exists()) {
            try {
                BufferedReader reader = new BufferedReader(new FileReader(file));
                FreeCamConfig config = gson.fromJson(reader, FreeCamConfig.class);
                config.clamp();
                reader.close();
                return config;
            } catch (Exception e) {
                logger.warn("Cannot read config", e);
            }
        }

        return new FreeCamConfig();
    }

    private File getFile() {
        File configDir = new File(Minecraft.getInstance().gameDirectory, "config");
        if (!configDir.exists()) {
            configDir.mkdirs();
        }

        return new File(configDir.getPath(), FILE);
    }
}
