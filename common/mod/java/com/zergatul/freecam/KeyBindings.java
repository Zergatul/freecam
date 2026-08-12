package com.zergatul.freecam;

import net.minecraft.client.settings.KeyBinding;
import net.minecraft.client.util.InputMappings;
import org.lwjgl.glfw.GLFW;

public class KeyBindings {

    public static final String CATEGORY = "category.zergatul.freecam";

    public static final KeyBinding TOGGLE_FREE_CAM = new KeyBinding("key.zergatul.freecam.toggle", GLFW.GLFW_KEY_F6, CATEGORY);
    public static final KeyBinding TOGGLE_CAMERA_LOCK = new KeyBinding("key.zergatul.freecam.cameralock.toggle", InputMappings.UNKNOWN.getValue(), CATEGORY);
    public static final KeyBinding TOGGLE_EYE_LOCK = new KeyBinding("key.zergatul.freecam.eyelock.toggle", InputMappings.UNKNOWN.getValue(), CATEGORY);
    public static final KeyBinding TOGGLE_FOLLOW_CAM = new KeyBinding("key.zergatul.freecam.followcam.toggle", InputMappings.UNKNOWN.getValue(), CATEGORY);

    private KeyBindings() {}
}