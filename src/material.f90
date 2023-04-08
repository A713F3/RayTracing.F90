module material_module
    use vec3_module
    use ray_module
    implicit none

    type material
        type(vec3) :: albedo
        character(len=3) :: mat_type
    end type material

contains

end module material_module