package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Minecraft;
import net.minecraftforge.client.GuiIngameForge;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GuiIngameForge.class)
public abstract class MixinGuiIngameForge {

    @Inject(method = "renderCrosshairs(II)V", at = @At("HEAD"), cancellable = true, require = 1, remap = false)
    private void freecam$renderCrosshair(int width, int height, CallbackInfo callback) {
        if (!FreeCam.INSTANCE.shouldRenderTarget()) {
            callback.cancel();
        }
    }

    @Inject(method = "renderHUDText(II)V", at = @At("RETURN"), require = 1, remap = false)
    private void freecam$renderDebugInfo(int width, int height, CallbackInfo callback) {
        FreeCam.INSTANCE.renderDebugInfo(Minecraft.getMinecraft().fontRendererObj);
    }
}