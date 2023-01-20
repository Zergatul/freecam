package com.zergatul.freecam;

import net.minecraft.client.settings.KeyBinding;
import net.minecraft.client.util.InputMappings;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import org.lwjgl.glfw.GLFW;

public class KeyBindingsController {

    public static final KeyBindingsController instance = new KeyBindingsController();

    public static final KeyBinding toggleFreeCam = new KeyBinding("key.zergatul.freecam.toggle", GLFW.GLFW_KEY_F6, "category.zergatul.freecam");
    public static final KeyBinding toggleCamControl = new KeyBinding("key.zergatul.freecam.camcontrol.toggle", InputMappings.UNKNOWN.getValue(), "category.zergatul.freecam");
    public static final KeyBinding toggleEyeLock = new KeyBinding("key.zergatul.freecam.eyelock.toggle", InputMappings.UNKNOWN.getValue(), "category.zergatul.freecam");

    private KeyBindingsController() {

    }

    public void setup() {
        ClientRegistry.registerKeyBinding(toggleFreeCam);
        ClientRegistry.registerKeyBinding(toggleCamControl);
        ClientRegistry.registerKeyBinding(toggleEyeLock);
    }
}
