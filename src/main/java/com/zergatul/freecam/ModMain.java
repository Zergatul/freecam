package com.zergatul.freecam;

import net.fabricmc.api.ClientModInitializer;

public class ModMain implements ClientModInitializer {
    @Override
    public void onInitializeClient() {
        KeyBindingsController.instance.setup();
        ModApiWrapper.instance.setup();
    }
}