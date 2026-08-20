package com.zergatul.freecam;

import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;
import net.fabricmc.fabric.api.client.rendering.v1.WorldRenderEvents;

public class ModMain implements ClientModInitializer {

    @Override
    public void onInitializeClient() {
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleFreeCam);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleCameraLock);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleEyeLock);
        KeyBindingHelper.registerKeyBinding(KeyBindings.toggleFollowCam);

        ClientTickEvents.START_CLIENT_TICK.register(client -> FreeCam.INSTANCE.onClientTickStart());
        WorldRenderEvents.START.register(context -> FreeCam.INSTANCE.onRenderTickStart(context.tickDelta()));
    }
}