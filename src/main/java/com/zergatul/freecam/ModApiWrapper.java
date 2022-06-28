package com.zergatul.freecam;

import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    private ModApiWrapper() {

    }

    public void setup() {
        ClientTickEvents.START_CLIENT_TICK.register(client -> {
            FreeCamController.instance.onClientTickStart();
        });
        ClientTickEvents.END_CLIENT_TICK.register(client -> {
            FreeCamController.instance.onKeyInput();
        });
    }

    public void onClientChatEvent(String message, Event event) {
        FreeCamController.instance.onClientChat(message, event);
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
