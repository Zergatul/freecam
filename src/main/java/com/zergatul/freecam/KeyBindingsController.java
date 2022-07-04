package com.zergatul.freecam;

import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();

    public static final KeyBinding toggleFreeCam = new KeyBinding("key.zergatul.freecam.toggle", GLFW.GLFW_KEY_F6, "category.zergatul.freecam");

    private KeyBindingsController() {

    }

    public void setup() {
        ClientRegistry.registerKeyBinding(toggleFreeCam);
    }
}
