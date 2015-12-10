--  This package is not really necessary for the build. It is here to satisfy
--  the fact that we declared in our main project file that we had Ada sources
--  as well a C sources (see comments in the project as to why we did that).
--  Without this package, we would have no Ada source.
package Pck is
   pragma Pure (Pck);
end Pck;
