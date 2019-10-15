using Atom

# calling Atom's serve()

# using Pkg
# Pkg.add("Atom")
# Pkg.activate("..")

# Atom.handle("ping") do
#   "pong"
# end

Atom.serve(5601)
# This should connect to the elisp side server
# Atom.connect(5602)


# using MacroTools

# @destruct [word, path || nothing] = [Dict("word"=>"hello"), Dict("path"=>nothing)]


