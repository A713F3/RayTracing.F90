module ray_module
    use vec3_module

    implicit none

    type ray
        type(vec3) :: o, d
    end type ray
end module ray_module