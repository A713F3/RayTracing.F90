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
        module procedure vmulv
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

    function vmulv(v1, v2) result(v3)
        type(vec3), intent(in) :: v1, v2
        type(vec3) :: v3

        v3 = vec3(v1%x * v2%x, v1%y * v2%y, v1%z * v2%z)
    end function vmulv

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

    function length_squared(v1)
        type(vec3), intent(in) :: v1
        real :: length_squared

        length_squared = v1%x**2 + v1%y**2 + v1%z**2
    end function length_squared

    function length(v1)
        type(vec3), intent(in) :: v1
        real :: length

        length = sqrt(length_squared(v1))
    end function

    function unit_vector(v)
        type(vec3), intent(in) :: v
        type(vec3) :: unit_vector
        real :: len

        len = length(v)

        unit_vector = v / len
    end function unit_vector

    function random_real(min, max)
        real, intent(in) :: min, max
        real :: random_real, r

        call random_number(r)
        random_real = min + (max-min) * r

    end function random_real

    function random_vec3(min, max)
        real, intent(in) :: min, max
        type(vec3) :: random_vec3

        random_vec3 = vec3(random_real(min,max), random_real(min,max), random_real(min,max))

    end function random_vec3

    function random_in_unit_sphere()
        type(vec3) :: random_in_unit_sphere

        do while (.true.)
            random_in_unit_sphere = random_vec3(-1.0, 1.0)
            if (length_squared(random_in_unit_sphere) .lt. 1.0) then
                return
            end if
        end do

    end function random_in_unit_sphere

    function random_unit_vector()
        type(vec3) :: random_unit_vector

        random_unit_vector = unit_vector(random_in_unit_sphere())
    end function random_unit_vector

    function near_zero(v)
        type(vec3), intent(in) :: v
        logical :: near_zero
        real :: s = 1e-8

        near_zero = (abs(v%x) .lt. s) .and. (abs(v%y) .lt. s) .and. (abs(v%z) .lt. s)
    end function near_zero

    function reflect(v, n)
        type(vec3), intent(in) :: v, n
        type(vec3) :: reflect

        reflect = v - n * (2.0 * dot(v, n))
    end function reflect

end module vec3_module