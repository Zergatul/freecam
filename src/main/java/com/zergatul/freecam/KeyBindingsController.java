package com.zergatul.freecam;

import org.lwjgl.input.Keyboard;

import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.fml.client.registry.ClientRegistry;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();

    public static final KeyBinding toggleFreeCam = new KeyBinding("key.zergatul.freecam.toggle", Keyboard.KEY_F6, "category.zergatul.freecam");

    public void setup() {
        ClientRegistry.registerKeyBinding(toggleFreeCam);
    }
}