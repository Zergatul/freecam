package com.zergatul.freecam.mixins;

import com.google.common.collect.Maps;
import io.netty.channel.ChannelHandler;
import net.minecraftforge.fml.common.ModContainer;
import net.minecraftforge.fml.common.network.FMLEmbeddedChannel;
import net.minecraftforge.fml.common.network.NetworkRegistry;
import net.minecraftforge.fml.relauncher.Side;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.EnumMap;
import java.util.Map;

@Mixin(value = NetworkRegistry.class, remap = false)
public abstract class MixinNetworkRegistry {

    @Shadow
    private EnumMap<Side, Map<String, FMLEmbeddedChannel>> channels;

    @Inject(at = @At("HEAD"), method = "newChannel(Lnet/minecraftforge/fml/common/ModContainer;Ljava/lang/String;[Lio/netty/channel/ChannelHandler;)Ljava/util/EnumMap;", remap = false)
    private void onNewChannel(ModContainer container, String name, ChannelHandler[] handlers, CallbackInfoReturnable<EnumMap<Side, FMLEmbeddedChannel>> info) {
        for (Side side : Side.values()) {
            if (channels.get(side) == null) {
                channels.put(side, Maps.<String,FMLEmbeddedChannel>newConcurrentMap());
            }
        }
    }
}