package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.renderer.ItemRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ItemRenderer.class)
public abstract class MixinItemRenderer {

    @Inject(
            method = "rotateWithPlayerRotations(Lnet/minecraft/client/entity/EntityPlayerSP;F)V",
            at = @At("HEAD"),
            cancellable = true)
    private void onRotateWithPlayerRotations(EntityPlayerSP player, float partialTicks, CallbackInfo info) {
        if (FreeCam.INSTANCE.shouldRenderHands()) {
            info.cancel();
        }
    }
}