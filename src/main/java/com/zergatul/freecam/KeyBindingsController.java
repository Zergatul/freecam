package com.zergatul.freecam;

import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;
import net.minecraft.client.option.KeyBinding;
import net.minecraft.client.util.InputUtil;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static KeyBinding toggleFreeCam;
    public static KeyBinding toggleCameraLock;
    public static KeyBinding toggleEyeLock;

    public static void setup() {
        toggleFreeCam = KeyBindingHelper.registerKeyBinding(new KeyBinding(
                "key.zergatul.freecam.toggle",
                GLFW.GLFW_KEY_F6,
                "category.zergatul.freecam"));
        toggleCameraLock = KeyBindingHelper.registerKeyBinding(new KeyBinding(
                "key.zergatul.freecam.cameralock.toggle",
                InputUtil.UNKNOWN_KEY.getCode(),
                "category.zergatul.freecam"));
        toggleEyeLock = KeyBindingHelper.registerKeyBinding(new KeyBinding(
                "key.zergatul.freecam.eyelock.toggle",
                InputUtil.UNKNOWN_KEY.getCode(),
                "category.zergatul.freecam"));
    }
}
