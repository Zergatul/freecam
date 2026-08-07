package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.renderer.RenderGlobal;
import net.minecraft.client.renderer.culling.ICamera;
import net.minecraft.entity.EntityLivingBase;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(RenderGlobal.class)
public abstract class MixinRenderGlobal {

    @Shadow
    private boolean occlusionEnabled;

    @Unique
    private boolean freecam$occlusionOverridden;

    @Unique
    private boolean freecam$savedOcclusionEnabled;

    @Inject(
            method = "sortAndRender(Lnet/minecraft/entity/EntityLivingBase;ID)I",
            at = @At("HEAD"),
            require = 1)
    private void freecam$beforeSortAndRender(
            EntityLivingBase viewEntity,
            int renderPass,
            double partialTicks,
            CallbackInfoReturnable<Integer> callback
    ) {
        if (FreeCam.INSTANCE.isActive()) {
            freecam$savedOcclusionEnabled = occlusionEnabled;
            occlusionEnabled = false;
            freecam$occlusionOverridden = true;
        }
    }

    @Inject(
            method = "sortAndRender(Lnet/minecraft/entity/EntityLivingBase;ID)I",
            at = @At("RETURN"),
            require = 1)
    private void freecam$afterSortAndRender(
            EntityLivingBase viewEntity,
            int renderPass,
            double partialTicks,
            CallbackInfoReturnable<Integer> callback
    ) {
        if (freecam$occlusionOverridden) {
            occlusionEnabled = freecam$savedOcclusionEnabled;
            freecam$occlusionOverridden = false;
        }
    }

    @Inject(
            method = "renderEntities(Lnet/minecraft/entity/EntityLivingBase;Lnet/minecraft/client/renderer/culling/ICamera;F)V",
            at = @At("HEAD"),
            require = 1)
    private void freecam$beforeRenderEntities(EntityLivingBase viewEntity, ICamera camera, float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onBeforeRenderEntities();
    }

    @Inject(
            method = "renderEntities(Lnet/minecraft/entity/EntityLivingBase;Lnet/minecraft/client/renderer/culling/ICamera;F)V",
            at = @At("RETURN"),
            require = 1)
    private void freecam$afterRenderEntities(EntityLivingBase viewEntity, ICamera camera, float partialTicks, CallbackInfo callback) {
        FreeCam.INSTANCE.onAfterRenderEntities();
    }
}