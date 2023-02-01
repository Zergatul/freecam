package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(RenderManager.class)
public abstract class MixinRenderManager {

    @Inject(at = @At("HEAD"), method = "renderEntitySimple(Lnet/minecraft/entity/Entity;F)Z", cancellable = true)
    private void onBeforeRenderEntitySimple(Entity entity, float partialTicks, CallbackInfoReturnable<Boolean> info) {
        FreeCamController.instance.onBeforeRenderEntity(entity);
    }

    @Inject(at = @At("TAIL"), method = "renderEntitySimple(Lnet/minecraft/entity/Entity;F)Z")
    private void onAfterRenderEntitySimple(Entity entity, float partialTicks, CallbackInfoReturnable<Boolean> info) {
        FreeCamController.instance.onAfterRenderEntity(entity);
    }
}