package com.zergatul.freecam;

import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.keymapping.v1.KeyMappingHelper;

public class ModMain implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFreeCam);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleCameraLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleEyeLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFollowCam);
        KeyMappingHelper.registerKeyMapping(KeyBindings.startPath);

        DebugScreenExtensions.register();
        ModApiWrapper.instance.setup();
    }
}