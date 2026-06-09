

## Import libraries
import geopandas as gpd
import rasterio
from rasterio.plot import show
from rasterio.mask import mask
import matplotlib.pyplot as plt
import numpy as np

## Clean plots
plt.close('all') 

## Load vector data
col1 = gpd.read_file('./gpkg/col1.gpkg')
world = gpd.read_file('./gpkg/world.gpkg')

## Function to make the map
def plot_mapa(raster_path, ax, cmap, title, cbar_label='', vmin=None, vmax=None, mostrar_leyenda=True):
    with rasterio.open(raster_path) as src:
        
        ## Projections
        col_proj = col1.to_crs(src.crs)
        world_proj = world.to_crs(src.crs)
        
        ## Read the raster
        data = src.read(1, masked=True)
        
        ## Layer 1 (Background - world)
        world_proj.plot(ax=ax, color='#e8e8e8', edgecolor='white', zorder=1)
        
        ## Layer 2 (raster)
        #geometrias = [geom for geom in col_proj.geometry]
        #data_recortada, transform_recortado = mask(src, geometrias, crop=True, filled=False)
        show(data, transform=src.transform, ax=ax, cmap=cmap, zorder=2, vmin=vmin, vmax=vmax)
        #show(data_recortada, transform=transform_recortado, ax=ax, cmap=cmap, zorder=2, vmin=vmin, vmax=vmax)

        ## Layer 3 (Administrative map)
        col_proj.plot(ax=ax, facecolor='none', edgecolor='#333333', linewidth=1.2, zorder=3)
        
        ## Extent [Colombia]
        bounds = col_proj.total_bounds
        ax.set_xlim(bounds[0], bounds[2]) #  - 1.5  + 1.5
        ax.set_ylim(bounds[1], bounds[3] ) # - 1.5
        
        ## Title
        ax.set_title(title, fontweight='bold', pad=10)
        ax.set_xlabel('Longitud')
        
        ## Individual leyend [precipitation]
        if mostrar_leyenda and ax.images:
            im = ax.images[0]
            cbar = plt.colorbar(im, ax=ax, orientation='vertical', shrink=0.7, pad=0.04, aspect=30)
            cbar.set_label(cbar_label, rotation=270, labelpad=15)

## Precipitation map
fig1, ax1 = plt.subplots(figsize = (8, 8))
plot_mapa('./tif/prec.tif', ax1, cmap = 'YlGnBu', title = 'Precipitación', cbar_label = 'Prec (mm)')

ax1.set_ylabel('Latitud')
plt.tight_layout()

### Guardar y mostrar
plt.savefig('mapa_precipitacion.png', dpi = 300, bbox_inches = 'tight')
plt.show()

## Temperature map
## Comun scale
with rasterio.open('./tif/tmin.tif') as src:
    tmin_data = src.read(1, masked=True)
    min1, max1 = tmin_data.min(), tmin_data.max()

with rasterio.open('./tif/tmax.tif') as src:
    tmax_data = src.read(1, masked=True)
    min2, max2 = tmax_data.min(), tmax_data.max()

global_min = min(min1, min2)
global_max = max(max1, max2)

# Comparative graph as facet_wrap
fig2, axes = plt.subplots(1, 2, figsize=(15, 7), sharey=True)

# Plot panels without legends
plot_mapa('./tif/tmin.tif', axes[0], cmap='coolwarm', title='Temperatura Mínima (Tmin)', 
          vmin=global_min, vmax=global_max, mostrar_leyenda=False)
axes[0].set_ylabel('Latitud')

plot_mapa('./tif/tmax.tif', axes[1], cmap='coolwarm', title='Temperatura Máxima (Tmax)', 
          vmin=global_min, vmax=global_max, mostrar_leyenda=False)
axes[1].set_ylabel('') # Limpiar etiqueta Y del panel derecho para que se vea limpio

# Configurar la leyenda común en su propio eje estricto
im = axes[0].images[0]

# Automatic layout
plt.tight_layout()

# Adjust
fig2.subplots_adjust(right=0.85)

# Fix legend
cbar_ax = fig2.add_axes([0.86, 0.15, 0.015, 0.7]) 

# Legend
cbar_comun = fig2.colorbar(im, cax=cbar_ax, orientation='vertical')
cbar_comun.set_label('°C', rotation=270, labelpad=15)

# Save the map [temperature]
plt.savefig('mapa_temperaturas_leyenda_comun.png', dpi=300, bbox_inches='tight')
plt.show()
