module vec3_module
    implicit none

    type vec3
        real :: x, y, z
    end type vec3

    interface operator (+)
        module procedure vadd
    end interface  

    interface operator (-)
        module procedure vsub
    end interface 

    interface operator (*)
        module procedure vmul
    end interface 

    interface operator ( / )
        module procedure vdiv
    end interface 

contains
    function vadd(v1, v2) result(v3)
        type(vec3), intent(in) :: v1, v2
        type(vec3) :: v3

        v3 = vec3(v1%x + v2%x, v1%y + v2%y, v1%z + v2%z)
    end function vadd

    function vsub(v1, v2) result(v3)
        type(vec3), intent(in) :: v1, v2
        type(vec3) :: v3

        v3 = vec3(v1%x - v2%x, v1%y - v2%y, v1%z - v2%z)
    end function vsub

    function vmul(v1, d) result(v2)
        type(vec3), intent(in) :: v1
        real, intent(in) :: d
        type(vec3) :: v2

        v2 = vec3(v1%x * d, v1%y * d, v1%z * d)
    end function vmul

    function vdiv(v1, d) result(v2)
        type(vec3), intent(in) :: v1
        real, intent(in) :: d
        type(vec3) :: v2

        v2 = vec3(v1%x / d, v1%y / d, v1%z / d)
    end function vdiv

    function normalize(v1) result(v2)
        type(vec3), intent(in) :: v1
        type(vec3) :: v2
        real :: mg

        mg = sqrt((v1%x ** 2) + (v1%y ** 2) + (v1%z ** 2))
        v2 = vec3(v1%x/mg, v1%y/mg, v1%z/mg)
    end function normalize

    function dot(v1, v2) result(d)
        type(vec3), intent(in) :: v1, v2
        real :: d

        d = (v1%x * v2%x) + (v1%y * v2%y) + (v1%z * v2%z)
    end function dot

    function clamp_val(t, d) result(clamped)
        real, intent(in) :: d, t
        real :: clamped

        if (d .gt. t) then
            clamped = t
        else 
            if (d .lt. 0.0) then
                clamped = 0.0
            else 
                clamped = d
            end if
        end if
    end function clamp_val

    function clamp(t, v1) result(v2)
        type(vec3), intent(in) :: v1
        real, intent(in) :: t
        type(vec3) :: v2

        v2 = vec3(clamp_val(t, v1%x), clamp_val(t, v1%y), clamp_val(t, v1%z))
    end function clamp

end module vec3_module