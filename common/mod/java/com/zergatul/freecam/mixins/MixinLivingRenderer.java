package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.freecam.helpers.MixinInventoryScreenHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.entity.LivingRenderer;
import net.minecraft.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(LivingRenderer.class)
public abstract class MixinLivingRenderer {

    @Inject(at = @At("RETURN"), method = "shouldShowName(Lnet/minecraft/entity/LivingEntity;)Z", cancellable = true)
    private void onShouldShowName(LivingEntity entity, CallbackInfoReturnable<Boolean> info) {
        if (FreeCam.INSTANCE.shouldShowMyName() && entity == Minecraft.getInstance().player && !MixinInventoryScreenHelper.renderingEntity) {
            info.setReturnValue(true);
        }
    }
}