package com.zergatul.freecam;

import com.zergatul.freecam.common.KeyBindings;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;

public class ModMain implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleFreeCam);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleCameraLock);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleEyeLock);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleFollowCam);
        KeyBindingHelper.registerKeyBinding(KeyBindings.startPath);

        ModApiWrapper.instance.setup();
    }
}