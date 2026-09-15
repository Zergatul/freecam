package com.zergatul.freecam.fabric;

import com.zergatul.freecam.*;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.keymapping.v1.KeyMappingHelper;

public class ModMain implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFreeCam);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleCameraLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleEyeLock);
        KeyMappingHelper.registerKeyMapping(KeyBindings.toggleFollowCam);

        DebugScreenExtensions.register();

        ClientTickEvents.START_CLIENT_TICK.register(_ -> FreeCam.INSTANCE.onClientTickStart());
    }
}