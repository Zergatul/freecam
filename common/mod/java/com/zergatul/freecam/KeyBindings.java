package com.zergatul.freecam;

import com.mojang.blaze3d.platform.InputConstants;
import net.minecraft.client.KeyMapping;
import net.minecraft.resources.ResourceLocation;
import org.lwjgl.glfw.GLFW;

public class KeyBindings {

    public static final KeyMapping.Category CATEGORY = KeyMapping.Category.register(ResourceLocation.fromNamespaceAndPath("freecam", "common"));

    public static final KeyMapping toggleFreeCam = new KeyMapping("key.zergatul.freecam.toggle", GLFW.GLFW_KEY_F6, CATEGORY);
    public static final KeyMapping toggleCameraLock = new KeyMapping("key.zergatul.freecam.cameralock.toggle", InputConstants.UNKNOWN.getValue(), CATEGORY);
    public static final KeyMapping toggleEyeLock = new KeyMapping("key.zergatul.freecam.eyelock.toggle", InputConstants.UNKNOWN.getValue(), CATEGORY);
    public static final KeyMapping toggleFollowCam = new KeyMapping("key.zergatul.freecam.followcam.toggle", InputConstants.UNKNOWN.getValue(), CATEGORY);
    public static final KeyMapping startPath = new KeyMapping("key.zergatul.freecam.start.path", InputConstants.UNKNOWN.getValue(), CATEGORY);
}