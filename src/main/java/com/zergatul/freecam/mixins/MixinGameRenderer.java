package com.zergatul.freecam.mixins;

import com.zergatul.freecam.helpers.MixinGameRendererHelper;
import net.minecraft.client.render.GameRenderer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/render/GameRenderer;updateTargetedEntity(F)V")
    private void onBeforeUpdateTargetedEntity(float tickDelta, CallbackInfo info) {
        MixinGameRendererHelper.insideUpdateTargetedEntity = true;
    }

    @Inject(at = @At("TAIL"), method = "Lnet/minecraft/client/render/GameRenderer;updateTargetedEntity(F)V")
    private void onAfterUpdateTargetedEntity(float tickDelta, CallbackInfo info) {
        MixinGameRendererHelper.insideUpdateTargetedEntity = false;
    }
}