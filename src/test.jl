using Flux.Data.MNIST
using Images: channelview
using Metalhead

using EmacsREPL: viewrepl

function test()
    # MNIST
    imgs = MNIST.images();
    size(imgs[1])               # 28,28
    typeof(imgs[1][1])          # ColorTypes.Gray
    viewrepl(imgs[1])

    # Converting to float
    typeof(float.(imgs[1])[1])     # Float64
    viewrepl(float.(imgs[1]))

    # CIFAR
    imgs = trainimgs(CIFAR10);
    size(imgs[1].img)           # 32,32
    typeof(imgs[1].img[1])      # RGB

    viewrepl(imgs[1].img)

    getarray(X) = Float32.(permutedims(channelview(X), (2, 3, 1)))
    getRGB(X) = colorview(RGB, permutedims(X, (3,1,2)))

    # converting to float and channel view
    floatimg = getarray(imgs[1].img);
    size(floatimg)              # 32,32,3
    typeof(floatimg[1])         # Float32
    viewrepl(floatimg)          # this is not working

    # converting back to RGB
    rgbimg = getRGB(floatimg);
    viewrepl(rgbimg)
end

