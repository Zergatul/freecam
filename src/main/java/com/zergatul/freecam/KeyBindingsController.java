package com.zergatul.freecam;

import net.minecraft.client.KeyMapping;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();

    public static final KeyMapping toggleFreeCam = new KeyMapping("key.zergatul.freecam.toggle", GLFW.GLFW_KEY_F6, "category.zergatul.freecam");

    private KeyBindingsController() {

    }
}
