package com.zergatul.freecam;

import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;
import net.minecraft.client.option.KeyBinding;
import net.minecraft.client.util.InputUtil;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();

    private KeyBinding keyBinding;

    public KeyBinding getKeyBinding() {
        return keyBinding;
    }

    public void setup() {
        keyBinding = KeyBindingHelper.registerKeyBinding(new KeyBinding(
                "key.zergatul.freecam.toggle",
                InputUtil.Type.KEYSYM,
                GLFW.GLFW_KEY_F6,
                "category.zergatul.freecam"
        ));
    }
}
