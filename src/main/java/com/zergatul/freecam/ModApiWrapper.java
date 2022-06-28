package com.zergatul.freecam;

import net.minecraftforge.client.event.ClientChatEvent;
import net.minecraftforge.client.event.InputEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    private ModApiWrapper() {

    }

    @SubscribeEvent
    public void onKeyInputEvent(InputEvent.KeyInputEvent event) {
        FreeCamController.instance.onKeyInput();
    }

    @SubscribeEvent
    public void onClientChatEvent(ClientChatEvent event) {
        Event evt = new Event();
        FreeCamController.instance.onClientChat(event.getMessage(), evt);
        if (evt.isCanceled()) {
            event.setCanceled(true);
        }
    }

    @SubscribeEvent
    public void onRenderTick(TickEvent.RenderTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            FreeCamController.instance.onRenderTickStart();
        }
    }

    @SubscribeEvent
    public void onClientTick(TickEvent.ClientTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            FreeCamController.instance.onClientTickStart();
        }
    }

    @SubscribeEvent
    public void onWorldUnload(WorldEvent.Unload event) {
        FreeCamController.instance.onWorldUnload();
    }

    public static class Event {

        private boolean canceled;

        public void cancel() {
            canceled = true;
        }

        public boolean isCanceled() {
            return canceled;
        }
    }
}
