import pandas as pd
import numpy as np
import time

def haversine(lat1, lon1, lat2, lon2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # Convert decimal degrees to radians
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
    
    # Haversine formula
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
    c = 2 * np.arcsin(np.sqrt(a))
    km = 6371 * c  # Radius of earth in kilometers
    return km

# Read the data
df = pd.read_excel('clinics.xls')

# Reference point
ref_lat, ref_lon = 38.6270, -90.1994

# Method 1: For loop (basic approach)
def calculate_distances_loop():
    distances = []
    start_time = time.time()
    
    for _, row in df.iterrows():
        dist = haversine(ref_lat, ref_lon, row['locLat'], row['locLong'])
        distances.append(dist)
    
    end_time = time.time()
    return distances, end_time - start_time

# Method 2: Vectorized with pandas apply
def calculate_distances_apply():
    start_time = time.time()
    distances = df.apply(lambda row: haversine(ref_lat, ref_lon, 
                                             row['locLat'], row['locLong']), axis=1)
    end_time = time.time()
    return distances, end_time - start_time

# Method 3: Fully vectorized with NumPy
def calculate_distances_vectorized():
    start_time = time.time()
    distances = haversine(ref_lat, ref_lon, 
                         df['locLat'].values, df['locLong'].values)
    end_time = time.time()
    return distances, end_time - start_time

# Run and time each method
distances_loop, time_loop = calculate_distances_loop()
distances_apply, time_apply = calculate_distances_apply()
distances_vectorized, time_vectorized = calculate_distances_vectorized()

print(f"For loop time: {time_loop:.4f} seconds")
print(f"Apply time: {time_apply:.4f} seconds")
print(f"Vectorized time: {time_vectorized:.4f} seconds")