package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.gui.GuiIngame;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(GuiIngame.class)
public abstract class MixinGuiIngame {

    @Inject(at = @At("HEAD"), method = "showCrosshair()Z", cancellable = true)
    private void onShowCrosshair(CallbackInfoReturnable<Boolean> info) {
        if (!FreeCam.INSTANCE.shouldRenderTarget()) {
            info.setReturnValue(false);
        }
    }
}