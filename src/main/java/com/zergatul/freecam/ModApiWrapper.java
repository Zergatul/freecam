package com.zergatul.freecam;

import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent;

public class ModApiWrapper {

    public static final ModApiWrapper instance = new ModApiWrapper();

    private ModApiWrapper() {}

    @SubscribeEvent
    public void onKeyInputEvent(InputEvent.KeyInputEvent event) {
        FreeCam.INSTANCE.onKeyInput();
    }

    @SubscribeEvent
    public void onRenderTick(TickEvent.RenderTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            FreeCam.INSTANCE.onRenderTickStart(event.renderTickTime);
        }
    }

    @SubscribeEvent
    public void onClientTick(TickEvent.ClientTickEvent event) {
        if (event.phase == TickEvent.Phase.START) {
            ChatCommandManager.INSTANCE.onClientTickStart();
            FreeCam.INSTANCE.onClientTickStart();
        }
    }

    /*@SubscribeEvent
    public void onWorldUnload(WorldEvent.Unload event) {
        FreeCamController.instance.onWorldUnload();
    }*/
}