package com.zergatul.freecam;

import net.minecraft.client.settings.KeyBinding;
import org.lwjgl.input.Keyboard;

public class KeyBindings {
    public static final KeyBinding toggleFreeCam = new KeyBinding(
            "key.zergatul.freecam.toggle",
            Keyboard.KEY_F6,
            "category.zergatul.freecam");
}