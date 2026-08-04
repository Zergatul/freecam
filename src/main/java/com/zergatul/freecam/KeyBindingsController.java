package com.zergatul.freecam;

import org.lwjgl.input.Keyboard;

import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.fml.client.registry.ClientRegistry;

public class KeyBindingsController {

    public static final KeyBindingsController INSTANCE = new KeyBindingsController();

    public static final KeyBinding toggleFreeCam = new KeyBinding("key.zergatul.freecam.toggle", Keyboard.KEY_F6, "category.zergatul.freecam");
    public static final KeyBinding toggleCameraLock = new KeyBinding("key.zergatul.freecam.cameralock.toggle", Keyboard.KEY_NONE, "category.zergatul.freecam");
    public static final KeyBinding toggleEyeLock = new KeyBinding("key.zergatul.freecam.eyelock.toggle", Keyboard.KEY_NONE, "category.zergatul.freecam");
    public static final KeyBinding toggleFollowCam = new KeyBinding("key.zergatul.freecam.followcam.toggle", Keyboard.KEY_NONE, "category.zergatul.freecam");

    public void setup() {
        ClientRegistry.registerKeyBinding(toggleFreeCam);
        ClientRegistry.registerKeyBinding(toggleCameraLock);
        ClientRegistry.registerKeyBinding(toggleEyeLock);
        ClientRegistry.registerKeyBinding(toggleFollowCam);
    }
}