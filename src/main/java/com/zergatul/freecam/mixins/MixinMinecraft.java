package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.Minecraft;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Minecraft.class)
public abstract class MixinMinecraft {

    @Inject(at = @At("TAIL"), method = "Lnet/minecraft/client/Minecraft;handleKeybinds()V")
    private void onHandleKeyBindings(CallbackInfo info) {
        FreeCamController.instance.onHandleKeyBindings();
    }
}