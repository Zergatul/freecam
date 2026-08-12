package com.zergatul.freecam.mixins;

import com.zergatul.freecam.helpers.MixinInventoryScreenHelper;
import net.minecraft.client.gui.screen.inventory.InventoryScreen;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(InventoryScreen.class)
public abstract class MixinInventoryScreen {

    @Inject(
            method = "renderEntityInInventory",
            at = @At(value = "INVOKE", target = "Lcom/mojang/blaze3d/systems/RenderSystem;runAsFancy(Ljava/lang/Runnable;)V"))
    private static void onBeforeRenderEntityInInventory(CallbackInfo ci) {
        MixinInventoryScreenHelper.renderingEntity = true;
    }

    @Inject(
            method = "renderEntityInInventory",
            at = @At(value = "INVOKE", target = "Lcom/mojang/blaze3d/systems/RenderSystem;runAsFancy(Ljava/lang/Runnable;)V", shift = At.Shift.AFTER))
    private static void onAfterRenderEntityInInventory(CallbackInfo ci) {
        MixinInventoryScreenHelper.renderingEntity = false;
    }
}