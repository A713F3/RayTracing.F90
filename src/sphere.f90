module sphere_module
    use vec3_module
    use ray_module

    implicit none

    type sphere 
        type(vec3) :: c
        real :: r
        type(vec3) :: color
    end type sphere
    
contains
    function get_normal(s, pi) result(normal)
        type(sphere), intent(in) :: s
        type(vec3), intent(in) :: pi
        type(vec3) :: normal

        normal = (pi - s%c) / s%r  

    end function get_normal

    subroutine intersect(s, r, t, intersects)
        type(sphere), intent(in) :: s
        type(ray), intent(in) :: r
        real, intent(out) :: t
        logical, intent(out) :: intersects

        type(vec3) :: oc
        real :: b, c, disc, t0, t1

        oc = r%o - s%c

        b = 2.0 * dot(oc, r%d)
        c = dot(oc, oc) - s%r ** 2
        disc = b ** 2 - 4.0 * c

        if (disc .lt. 0.0001) then
            intersects = .false.
        else
            disc = sqrt(disc)

            t0 = -b - disc
            t1 = -b + disc

            t = min(t0, t1)

            intersects = .true.
        end if

    end subroutine intersect
    
end module sphere_module