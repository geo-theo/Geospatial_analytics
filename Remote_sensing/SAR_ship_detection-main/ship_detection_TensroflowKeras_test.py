def image_batch_generators(
    train_df, dev_df, target_size=(256, 256), input_dir="../../datasets/satellite_ships"
):
    train_datagen = tf.keras.preprocessing.image.ImageDataGenerator(
        rescale=1.0 / 255,
        horizontal_flip=True,
        vertical_flip=True,
    )

    test_datagen = tf.keras.preprocessing.image.ImageDataGenerator(rescale=1.0 / 255)

    train_generator = train_datagen.flow_from_dataframe(
        dataframe=train_df,
        directory=input_dir + "/train_v2/",
        x_col="ImageId",
        y_col="has_vessel_str",
        target_size=target_size,
        batch_size=40,
        class_mode="binary",
    )

    validation_generator = test_datagen.flow_from_dataframe(
        dataframe=dev_df,
        directory=input_dir + "/train_v2/",
        x_col="ImageId",
        y_col="has_vessel_str",
        target_size=target_size,
        batch_size=40,
        class_mode="binary",
    )

    return train_generator, validation_generator

#####

def define_model_supersimple_convnet(IMG_HEIGHT=256, IMG_WIDTH=256):
    model = tf.keras.Sequential(
        [
            keras.layers.Conv2D(
                16,
                3,
                padding="same",
                activation="relu",
                input_shape=(IMG_HEIGHT, IMG_WIDTH, 3),
            ),
            tf.keras.layers.MaxPooling2D(),
            tf.keras.layers.Conv2D(32, 3, padding="same", activation="relu"),
            tf.keras.layers.MaxPooling2D(),
            tf.keras.layers.Conv2D(64, 3, padding="same", activation="relu"),
            tf.keras.layers.MaxPooling2D(),
            tf.keras.layers.Dropout(0.2),
            tf.keras.layers.Flatten(),
            tf.keras.layers.Dense(128, activation="relu"),
            tf.keras.layers.Dense(2, activation="softmax")
        ]
    )

    model.compile(
        optimizer=tf.keras.optimizers.Adam(
            learning_rate=3e-4
        ),  # this LR is overriden by base cycle LR if CyclicLR callback used
        loss="sparse_categorical_crossentropy",
        metrics=["accuracy"],
    )

    print(model.summary())

    return model


#####

# update: fit_generator will be deprecated: use fit instead -> apparently works faster with Tensorflow 2.0
history = model.fit_generator(
  train_generator,
  steps_per_epoch=1000,
  epochs=35,
  validation_data=validation_generator,
  validation_steps=100,
  callbacks=[
      cp_callback,
      #             lr_finder,
      #             cyclic_learning_rate,
      #              tensorboard_callback
  ],
)

