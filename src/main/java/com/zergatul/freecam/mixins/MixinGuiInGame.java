package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.gui.GuiIngame;
import net.minecraft.client.gui.ScaledResolution;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GuiIngame.class)
public abstract class MixinGuiInGame {

    @Inject(at = @At("HEAD"), method = "renderAttackIndicator(FLnet/minecraft/client/gui/ScaledResolution;)V", cancellable = true)
    private void onRenderCrosshair(float partialTicks, ScaledResolution resolution, CallbackInfo info) {
        if (!FreeCam.instance.shouldRenderCrosshair()) {
            info.cancel();
        }
    }
}