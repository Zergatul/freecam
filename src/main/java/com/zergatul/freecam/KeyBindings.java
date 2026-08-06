package com.zergatul.freecam;

import net.minecraft.client.settings.KeyBinding;
import org.lwjgl.input.Keyboard;

public class KeyBindings {
    public static final KeyBinding toggleFreeCam = new KeyBinding(
            "key.zergatul.freecam.toggle",
            Keyboard.KEY_F6,
            "category.zergatul.freecam");

    public static final KeyBinding toggleCameraLock = new KeyBinding(
            "key.zergatul.freecam.cameralock.toggle",
            Keyboard.KEY_NONE,
            "category.zergatul.freecam");

    public static final KeyBinding toggleEyeLock = new KeyBinding(
            "key.zergatul.freecam.eyelock.toggle",
            Keyboard.KEY_NONE,
            "category.zergatul.freecam");

    public static final KeyBinding toggleFollowCam = new KeyBinding(
            "key.zergatul.freecam.followcam.toggle",
            Keyboard.KEY_NONE,
            "category.zergatul.freecam");
}