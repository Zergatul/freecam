package com.zergatul.freecam;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.*;

public class ConfigRepository {

    public static final ConfigRepository INSTANCE = new ConfigRepository();

    private static final String FILE = ModMain.MOD_ID + ".json";

    private final Logger logger = LogManager.getLogger(ConfigRepository.class);
    private final Gson gson = new GsonBuilder()
            .setPrettyPrinting()
            .create();

    private File file;
    private FreeCamConfig config = new FreeCamConfig();

    private ConfigRepository() {}

    public void init(File directory) {
        file = getFile(directory);
        if (file.exists()) {
            try (FileReader fileReader = new FileReader(file)) {
                BufferedReader reader = new BufferedReader(fileReader);
                config = gson.fromJson(reader, FreeCamConfig.class);
                config.clamp();
                reader.close();
            } catch (Exception e) {
                logger.warn("Cannot read config", e);
            }
        }
    }

    public FreeCamConfig getConfig() {
        return config;
    }

    public void save() {
        try (FileWriter fileWriter = new FileWriter(file)) {
            BufferedWriter writer = new BufferedWriter(fileWriter);
            gson.toJson(config, writer);
            writer.close();
        } catch (Throwable e) {
            logger.warn("Cannot write config", e);
        }
    }

    private File getFile(File directory) {
        File configDir = new File(directory, "config");
        if (!configDir.exists()) {
            configDir.mkdirs();
        }

        return new File(configDir.getPath(), FILE);
    }
}