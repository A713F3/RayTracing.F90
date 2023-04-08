module sphere_module
    use vec3_module
    use ray_module
    use material_module

    implicit none

    type sphere 
        type(vec3) :: center
        real :: radius
        type(material) :: mat
    end type sphere
    
contains
    function get_normal(s, pi) result(normal)
        type(sphere), intent(in) :: s
        type(vec3), intent(in) :: pi
        type(vec3) :: normal

        normal = (pi - s%center) / s%radius 

    end function get_normal

end module sphere_module