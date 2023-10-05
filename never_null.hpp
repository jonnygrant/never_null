// g++ -g -Wall -O2 -o not_null2 not_null2.cpp

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2015 Microsoft Corporation. All rights reserved.
//
// This code is licensed under the MIT License (MIT).
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef GSL_ASSERT_H
#define GSL_ASSERT_H
#include <cstdio>
//
// Temporary until MSVC STL supports no-exceptions mode.
// Currently terminate is a no-op in this mode, so we add termination behavior back
//
#if defined(_MSC_VER) && (defined(_KERNEL_MODE) || (defined(_HAS_EXCEPTIONS) && !_HAS_EXCEPTIONS))
#define GSL_KERNEL_MODE

#define GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND
#include <intrin.h>
#define RANGE_CHECKS_FAILURE 0

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-noreturn"
#endif // defined(__clang__)

#else // defined(_MSC_VER) && (defined(_KERNEL_MODE) || (defined(_HAS_EXCEPTIONS) &&
      // !_HAS_EXCEPTIONS))

#include <exception>

#endif // defined(_MSC_VER) && (defined(_KERNEL_MODE) || (defined(_HAS_EXCEPTIONS) &&
       // !_HAS_EXCEPTIONS))

//
// make suppress attributes parse for some compilers
// Hopefully temporary until suppression standardization occurs
//
#if defined(__clang__)
#define GSL_SUPPRESS(x) [[gsl::suppress("x")]]
#else
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) && !defined(__NVCC__)
#define GSL_SUPPRESS(x) [[gsl::suppress(x)]]
#else
#define GSL_SUPPRESS(x)
#endif // _MSC_VER
#endif // __clang__

#define GSL_STRINGIFY_DETAIL(x) #x
#define GSL_STRINGIFY(x) GSL_STRINGIFY_DETAIL(x)

#if defined(__clang__) || defined(__GNUC__)
#define GSL_LIKELY(x) __builtin_expect(!!(x), 1)
#define GSL_UNLIKELY(x) __builtin_expect(!!(x), 0)

#else

#define GSL_LIKELY(x) (!!(x))
#define GSL_UNLIKELY(x) (!!(x))
#endif // defined(__clang__) || defined(__GNUC__)

//
// GSL_ASSUME(cond)
//
// Tell the optimizer that the predicate cond must hold. It is unspecified
// whether or not cond is actually evaluated.
//
#ifdef _MSC_VER
#define GSL_ASSUME(cond) __assume(cond)
#elif defined(__GNUC__)
#define GSL_ASSUME(cond) ((cond) ? static_cast<void>(0) : __builtin_unreachable())
#else
#define GSL_ASSUME(cond) static_cast<void>((cond) ? 0 : 0)
#endif

//
// GSL.assert: assertions
//

namespace gsl
{

namespace details
{
#if defined(GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND)

    typedef void(__cdecl* terminate_handler)();

    // clang-format off
    GSL_SUPPRESS(f.6) // NO-FORMAT: attribute
    // clang-format on
    [[noreturn]] inline void __cdecl default_terminate_handler()
    {
        __fastfail(RANGE_CHECKS_FAILURE);
    }

    inline gsl::details::terminate_handler& get_terminate_handler() noexcept
    {
        static terminate_handler handler = &default_terminate_handler;
        return handler;
    }

#endif // defined(GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND)

    [[noreturn]] inline void terminate() noexcept
    {
#if defined(GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND)
        (*gsl::details::get_terminate_handler())();
#else
        std::terminate();
#endif // defined(GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND)
    }

} // namespace details
} // namespace gsl

#define GSL_CONTRACT_CHECK(type, cond)                                                             \
    (GSL_LIKELY(cond) ? static_cast<void>(0) : gsl::details::terminate())

#define Expects(cond) GSL_CONTRACT_CHECK("Precondition", cond)
#define Ensures(cond) GSL_CONTRACT_CHECK("Postcondition", cond)

#if defined(GSL_MSVC_USE_STL_NOEXCEPTION_WORKAROUND) && defined(__clang__)
#pragma clang diagnostic pop
#endif

#endif // GSL_ASSERT_H

#ifndef GSL_POINTERS_H
#define GSL_POINTERS_H


#include <algorithm>    // for forward
#include <cstddef>      // for ptrdiff_t, nullptr_t, size_t
#include <memory>       // for shared_ptr, unique_ptr
#include <system_error> // for hash
#include <type_traits>  // for enable_if_t, is_convertible, is_assignable

#if !defined(GSL_NO_IOSTREAMS)
#include <iosfwd> // for ostream
#endif            // !defined(GSL_NO_IOSTREAMS)

namespace gsl
{

namespace details
{
    template <typename T, typename = void>
    struct is_comparable_to_nullptr : std::false_type
    {
    };

    template <typename T>
    struct is_comparable_to_nullptr<
        T,
        std::enable_if_t<std::is_convertible<decltype(std::declval<T>() != nullptr), bool>::value>>
        : std::true_type
    {
    };

    // Resolves to the more efficient of `const T` or `const T&`, in the context of returning a const-qualified value
    // of type T.
    //
    // Copied from cppfront's implementation of the CppCoreGuidelines F.16 (https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Rf-in)
    template<typename T>
    using value_or_reference_return_t = std::conditional_t<
                                            sizeof(T) < 2*sizeof(void*) && std::is_trivially_copy_constructible<T>::value,
                                            const T,
                                            const T&>;

} // namespace details

void compile_error_null_check() __attribute__ ((error("not_null found nullptr")));


//
// GSL.owner: ownership pointers
//
using std::shared_ptr;
using std::unique_ptr;

//
// owner
//
// `gsl::owner<T>` is designed as a safety mechanism for code that must deal directly with raw pointers that own memory.
// Ideally such code should be restricted to the implementation of low-level abstractions. `gsl::owner` can also be used
// as a stepping point in converting legacy code to use more modern RAII constructs, such as smart pointers.
//
// T must be a pointer type
// - disallow construction from any type other than pointer type
//
template <class T, class = std::enable_if_t<std::is_pointer<T>::value>>
using owner = T;

//
// not_null
//
// Restricts a pointer or smart pointer to only hold non-null values.
//
// Has zero size overhead over T.
//
// If T is a pointer (i.e. T == U*) then
// - allow construction from U*
// - disallow construction from nullptr_t
// - disallow default construction
// - ensure construction from null U* fails
// - allow implicit conversion to U*
//
template <class T>
class not_null
{
public:
    static_assert(details::is_comparable_to_nullptr<T>::value, "T cannot be compared to nullptr.");

    template <typename U, typename = std::enable_if_t<std::is_convertible<U, T>::value>>
    constexpr not_null(U&& u) : ptr_(std::forward<U>(u))
    {
#ifdef __OPTIMIZE__
        if(!ptr_) compile_error_null_check();
#endif
        // This is the runtime check that calls std::terminate
        Expects(ptr_ != nullptr);
    }

    template <typename = std::enable_if_t<!std::is_same<std::nullptr_t, T>::value>>
    constexpr not_null(T u) : ptr_(std::move(u))
    {
#ifdef __OPTIMIZE__
        if(!ptr_) compile_error_null_check();
#endif
        // This is the runtime check that calls std::terminate
        Expects(ptr_ != nullptr);
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible<U, T>::value>>
    constexpr not_null(const not_null<U>& other) : not_null(other.get())
    {}

    not_null(const not_null& other) = default;
    not_null& operator=(const not_null& other) = default;
    constexpr details::value_or_reference_return_t<T> get() const
    {
        return ptr_;
    }

    constexpr operator T() const { return get(); }
    constexpr decltype(auto) operator->() const { return get(); }
    constexpr decltype(auto) operator*() const { return *get(); }

    // prevents compilation when someone attempts to assign a null pointer constant
    not_null(std::nullptr_t) = delete;
    not_null& operator=(std::nullptr_t) = delete;

    // unwanted operators...pointers only point to single objects!
    not_null& operator++() = delete;
    not_null& operator--() = delete;
    not_null operator++(int) = delete;
    not_null operator--(int) = delete;
    not_null& operator+=(std::ptrdiff_t) = delete;
    not_null& operator-=(std::ptrdiff_t) = delete;
    void operator[](std::ptrdiff_t) const = delete;

private:
    T ptr_;
};

template <class T>
auto make_not_null(T&& t) noexcept
{
    return not_null<std::remove_cv_t<std::remove_reference_t<T>>>{std::forward<T>(t)};
}

#if !defined(GSL_NO_IOSTREAMS)
template <class T>
std::ostream& operator<<(std::ostream& os, const not_null<T>& val)
{
    os << val.get();
    return os;
}
#endif // !defined(GSL_NO_IOSTREAMS)

template <class T, class U>
auto operator==(const not_null<T>& lhs,
                const not_null<U>& rhs) noexcept(noexcept(lhs.get() == rhs.get()))
    -> decltype(lhs.get() == rhs.get())
{
    return lhs.get() == rhs.get();
}

template <class T, class U>
auto operator!=(const not_null<T>& lhs,
                const not_null<U>& rhs) noexcept(noexcept(lhs.get() != rhs.get()))
    -> decltype(lhs.get() != rhs.get())
{
    return lhs.get() != rhs.get();
}

template <class T, class U>
auto operator<(const not_null<T>& lhs,
               const not_null<U>& rhs) noexcept(noexcept(lhs.get() < rhs.get()))
    -> decltype(lhs.get() < rhs.get())
{
    return lhs.get() < rhs.get();
}

template <class T, class U>
auto operator<=(const not_null<T>& lhs,
                const not_null<U>& rhs) noexcept(noexcept(lhs.get() <= rhs.get()))
    -> decltype(lhs.get() <= rhs.get())
{
    return lhs.get() <= rhs.get();
}

template <class T, class U>
auto operator>(const not_null<T>& lhs,
               const not_null<U>& rhs) noexcept(noexcept(lhs.get() > rhs.get()))
    -> decltype(lhs.get() > rhs.get())
{
    return lhs.get() > rhs.get();
}

template <class T, class U>
auto operator>=(const not_null<T>& lhs,
                const not_null<U>& rhs) noexcept(noexcept(lhs.get() >= rhs.get()))
    -> decltype(lhs.get() >= rhs.get())
{
    return lhs.get() >= rhs.get();
}

// more unwanted operators
template <class T, class U>
std::ptrdiff_t operator-(const not_null<T>&, const not_null<U>&) = delete;
template <class T>
not_null<T> operator-(const not_null<T>&, std::ptrdiff_t) = delete;
template <class T>
not_null<T> operator+(const not_null<T>&, std::ptrdiff_t) = delete;
template <class T>
not_null<T> operator+(std::ptrdiff_t, const not_null<T>&) = delete;

} // namespace gsl

namespace std
{
template <class T>
struct hash<gsl::not_null<T>>
{
    std::size_t operator()(const gsl::not_null<T>& value) const { return hash<T>{}(value.get()); }
};

} // namespace std

namespace gsl
{

//
// strict_not_null
//
// Restricts a pointer or smart pointer to only hold non-null values,
//
// - provides a strict (i.e. explicit constructor from T) wrapper of not_null
// - to be used for new code that wishes the design to be cleaner and make not_null
//   checks intentional, or in old code that would like to make the transition.
//
//   To make the transition from not_null, incrementally replace not_null
//   by strict_not_null and fix compilation errors
//
//   Expect to
//   - remove all unneeded conversions from raw pointer to not_null and back
//   - make API clear by specifying not_null in parameters where needed
//   - remove unnecessary asserts
//
template <class T>
class strict_not_null : public not_null<T>
{
public:
    template <typename U, typename = std::enable_if_t<std::is_convertible<U, T>::value>>
    constexpr explicit strict_not_null(U&& u) : not_null<T>(std::forward<U>(u))
    {}

    template <typename = std::enable_if_t<!std::is_same<std::nullptr_t, T>::value>>
    constexpr explicit strict_not_null(T u) : not_null<T>(u)
    {}

    template <typename U, typename = std::enable_if_t<std::is_convertible<U, T>::value>>
    constexpr strict_not_null(const not_null<U>& other) : not_null<T>(other)
    {}

    template <typename U, typename = std::enable_if_t<std::is_convertible<U, T>::value>>
    constexpr strict_not_null(const strict_not_null<U>& other) : not_null<T>(other)
    {}

    // To avoid invalidating the "not null" invariant, the contained pointer is actually copied
    // instead of moved. If it is a custom pointer, its constructor could in theory throw exceptions.
    strict_not_null(strict_not_null&& other) noexcept(std::is_nothrow_copy_constructible<T>::value) = default;
    strict_not_null(const strict_not_null& other) = default;
    strict_not_null& operator=(const strict_not_null& other) = default;
    strict_not_null& operator=(const not_null<T>& other)
    {
        not_null<T>::operator=(other);
        return *this;
    }

    // prevents compilation when someone attempts to assign a null pointer constant
    strict_not_null(std::nullptr_t) = delete;
    strict_not_null& operator=(std::nullptr_t) = delete;

    // unwanted operators...pointers only point to single objects!
    strict_not_null& operator++() = delete;
    strict_not_null& operator--() = delete;
    strict_not_null operator++(int) = delete;
    strict_not_null operator--(int) = delete;
    strict_not_null& operator+=(std::ptrdiff_t) = delete;
    strict_not_null& operator-=(std::ptrdiff_t) = delete;
    void operator[](std::ptrdiff_t) const = delete;
};

// more unwanted operators
template <class T, class U>
std::ptrdiff_t operator-(const strict_not_null<T>&, const strict_not_null<U>&) = delete;
template <class T>
strict_not_null<T> operator-(const strict_not_null<T>&, std::ptrdiff_t) = delete;
template <class T>
strict_not_null<T> operator+(const strict_not_null<T>&, std::ptrdiff_t) = delete;
template <class T>
strict_not_null<T> operator+(std::ptrdiff_t, const strict_not_null<T>&) = delete;

template <class T>
auto make_strict_not_null(T&& t) noexcept
{
    return strict_not_null<std::remove_cv_t<std::remove_reference_t<T>>>{std::forward<T>(t)};
}

#if (defined(__cpp_deduction_guides) && (__cpp_deduction_guides >= 201611L))

// deduction guides to prevent the ctad-maybe-unsupported warning
template <class T>
not_null(T) -> not_null<T>;
template <class T>
strict_not_null(T) -> strict_not_null<T>;

#endif // ( defined(__cpp_deduction_guides) && (__cpp_deduction_guides >= 201611L) )

} // namespace gsl

namespace std
{
template <class T>
struct hash<gsl::strict_not_null<T>>
{
    std::size_t operator()(const gsl::strict_not_null<T>& value) const
    {
        return hash<T>{}(value.get());
    }
};

} // namespace std

#endif // GSL_POINTERS_H

//void f(gsl::not_null<int*> test)

