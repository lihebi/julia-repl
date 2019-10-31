module EmacsREPL

using FileIO
using FileIO: @format_str
using Images: channelview, colorview, RGB
# using Printf

export viewrepl

greet() = print("Hello World!")

# I have to import it before extending it, this is used for debugging
# import EmacsREPL.viewrepl

function viewrepl(img::Array{Float32,3})
    getRGB(X) = colorview(RGB, permutedims(X, (3,1,2)))
    rgbimg = getRGB(img);
    viewrepl(rgbimg)
end

# I need to customize based on the type of img
function viewrepl(img)
    # save("/tmp/a.png")
    path, io = Base.Filesystem.mktemp()
    # FileIO.save(FileIO.Stream(format"PNG", io), img)
    # I'm using .png suffix to hopefully make Emacs' life easier
    # The io variable is ignored, merely used for avoiding race condtion
    FileIO.save(File(format"PNG", path * ".png"), img)
    println("$(path).png")
    println("#<Image: $(path).png>")

    # If saved without .png suffix, and using the path, it is saved as
    # "imagemagick format", and cannot be displayed by 'png, but
    # 'imagemagick will be fast.
    # 
    # FileIO.save(File(format"PNG", path), img)

    # If otherwise saved by using io variable, the image is saved in
    # 'png format, and imagemagick will be very slow. So just do NOT
    # use this.
    #
    # save(Stream(format"PNG", io), img)

    # s = @sprintf("#<Image: %s>", path * ".png")

    # The string interpolation just works
    # println(path)
    # println("#<Image: $(path)>")
    # what to return?
    nothing
end

end # module
