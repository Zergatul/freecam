package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(RenderManager.class)
public abstract class MixinRenderManager {

    @Inject(
            method = "renderEntitySimple(Lnet/minecraft/entity/Entity;F)Z",
            at = @At("HEAD"),
            require = 1)
    private void freecam$beforeRenderEntity(Entity entity, float partialTicks, CallbackInfoReturnable<Boolean> callback) {
        FreeCam.INSTANCE.onBeforeRenderEntity(entity);
    }

    @Inject(
            method = "renderEntitySimple(Lnet/minecraft/entity/Entity;F)Z",
            at = @At("RETURN"),
            require = 1)
    private void freecam$afterRenderEntity(Entity entity, float partialTicks, CallbackInfoReturnable<Boolean> callback) {
        FreeCam.INSTANCE.onAfterRenderEntity(entity);
    }
}