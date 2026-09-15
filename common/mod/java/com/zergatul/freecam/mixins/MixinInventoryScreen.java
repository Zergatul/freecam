package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(InventoryScreen.class)
public abstract class MixinInventoryScreen {

    @Inject(method = "extractRenderState(Lnet/minecraft/world/entity/LivingEntity;)Lnet/minecraft/client/renderer/entity/state/EntityRenderState;", at = @At("HEAD"))
    private static void onBeforeExtractEntityRenderState(LivingEntity entity, CallbackInfoReturnable<EntityRenderState> cir) {
        FreeCam.INSTANCE.onBeforeRenderPlayerInInventory();
    }

    @Inject(method = "extractRenderState(Lnet/minecraft/world/entity/LivingEntity;)Lnet/minecraft/client/renderer/entity/state/EntityRenderState;", at = @At("RETURN"))
    private static void onAfterExtractEntityRenderState(LivingEntity entity, CallbackInfoReturnable<EntityRenderState> cir) {
        FreeCam.INSTANCE.onAfterRenderPlayerInInventory();
    }
}