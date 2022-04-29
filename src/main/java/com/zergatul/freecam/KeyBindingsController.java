package com.zergatul.freecam;

import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.minecraftforge.client.ClientRegistry;
import net.minecraftforge.client.event.InputEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();
    public static final String category = "FreeCam by Zergatul";

    public static KeyMapping toggleFreeCam = new KeyMapping("Toggle free cam", GLFW.GLFW_KEY_F6, category);

    private Minecraft mc = Minecraft.getInstance();

    private KeyBindingsController() {

    }

    public void setup() {
        ClientRegistry.registerKeyBinding(toggleFreeCam);
    }

    @SubscribeEvent
    public void onKeyInputEvent(InputEvent.KeyInputEvent event) {

        if (mc.player == null) {
            return;
        }
        if (mc.screen != null) {
            return;
        }

        if (KeyBindingsController.toggleFreeCam.isDown()) {
            FreeCamController.instance.toggle();
        }
    }
}
