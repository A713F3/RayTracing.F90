module ray_module
    use vec3_module

    implicit none

    type ray
        type(vec3) :: origin, direction
    end type ray

contains 
    function at(r, t)
        type(ray), intent(in) :: r
        real, intent(in) :: t
        type(vec3) :: at

        at = r%origin + (r%direction*t)

    end function

end module ray_module